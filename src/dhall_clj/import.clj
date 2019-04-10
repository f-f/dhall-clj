(ns dhall-clj.import
  (:require [dhall-clj.ast :refer :all]
            [medley.core :refer [map-vals]]
            [digest :refer [sha-256]]
            [me.raynes.fs :as fs]
            [qbits.ex :as ex]
            [clojure.java.io :as io]
            [clojure.data.finger-tree :refer [double-list conjl]]
            [clojure.string :as str]
            [dhall-clj.parse :refer [parse expr]]
            [dhall-clj.fail :as fail]
            [dhall-clj.alpha-normalize :refer [alpha-normalize]]
            [dhall-clj.beta-normalize :refer [beta-normalize]]
            [dhall-clj.typecheck :refer [typecheck]]
            [dhall-clj.binary :as binary]
            [dhall-clj.state :as s])
  (:import [dhall_clj.ast Import Local Remote Env Missing]))


;;
;; Canonicalize imports
;;

(defn canonicalize-dir [directory]
  (cond
    (empty? directory)         directory
    (= "."  (first directory)) (canonicalize-dir (rest directory))
    (= ".." (first directory)) (let [comps (canonicalize-dir (rest directory))]
                                 (cond
                                   (empty? comps)         '("..")
                                   (= ".." (first comps)) (conj comps "..")
                                   :else                  (rest comps)))
    :else                      (conj (canonicalize-dir (rest directory))
                                     (first directory))))


(defprotocol IFetch
  (canonicalize [this]
    "Normalize the path position of directories/paths.")
  (chain [this stack]
    "Compute the position of the current (possibly relative) import
    given the `stack` of parent imports.")
  (fetch [this]
    "Perform the side effect of fetching the import from its source.
    Returns a string or throws an `imports` exception."))

(extend-protocol IFetch
  Missing
  (canonicalize [this] this)
  (chain [this _stack] this)
  (fetch [_this]
    (fail/missing-keyword!))

  Env
  (canonicalize [this] this)
  (chain [this _stack] this)
  (fetch [{:keys [name]}]
    (if-let [env (System/getenv name)]
      env
      (fail/missing-env! name)))

  Local
  (canonicalize [this]
    (update this :directory canonicalize-dir))
  (chain [{:keys [prefix? directory file] :as this} stack]
    (let [parent (first stack)]
      (cond
        ;; TODO Remote parents?
        (and (instance? Local parent)
             (= "." prefix?))
        (-> parent
           (assoc :file file
                  :directory (concat directory
                                     (:directory parent)))
           (canonicalize))

        ;; TODO Remote parents?
        (and (instance? Local parent)
             (= ".." prefix?))
        (-> parent
           (assoc :file file
                  :directory (concat directory
                                     (conj (:directory parent) "..")))
           (canonicalize))

        :else this)))
  (fetch [{:keys [prefix? directory file] :as this}]
    (let [prefix (case prefix?
                   "~"  (fs/home)
                   "."  fs/*cwd*
                   ".." (io/file fs/*cwd* "..")
                   nil)
          filepath (io/file
                     prefix
                     (apply io/file (reverse (conj directory ".")))
                     file)]
      (if (fs/exists? filepath)
        (slurp filepath)
        (fail/missing-file! filepath this)))))

;;
;; Resolving imports
;;


(defn assert-dir
  "Ensure that `directory` exists and it's readable and writable"
  [directory]
  (if (and (fs/exists? directory)
           (fs/directory? directory))
    (assert (and (fs/readable? directory)
                 (fs/writeable? directory))
            "Directory should be readable and writeable")
    (do
      (assert-dir (fs/parent directory))
      (fs/mkdir directory)
      (fs/chmod "+rw" directory))))

(defn get-cached-file
  "Given a hash, returns the filename after making sure it's writable and stuff"
  [hash]
  (let [cache-dir (or (some-> (System/getenv "XDG_CACHE_HOME") io/file)
                      (io/file (fs/home) ".cache"))
        _ (assert-dir cache-dir)
        dhall-dir (io/file cache-dir "dhall")
        _ (assert-dir dhall-dir)]
    (io/file dhall-dir hash)))


(defn expr-from-import
  "Given a Missing, Env, Local or Remote, fetches them
  from the appropriate place, and returns and expression
  (that might contain more imports)."
  [import-data mode hash?]
  ;; Here we try to get the cached expression.
  ;; If we don't find it, we `fetch` a new one
  (if-let [cached (some->
                    hash?
                    get-cached-file
                    (#(when (fs/exists? %) %))
                    binary/slurp-bytes
                    ((fn [contents]
                       (let [actual-hash (sha-256 contents)]
                         (if (= actual-hash hash?)
                           contents
                           (fail/hash-mismatch! import-data actual-hash (get-cached-file hash?))))))
                    binary/decode)]
    cached
    (let [raw (fetch import-data)]
      (if (= mode :code)
        (-> raw parse expr)
        (->TextLit [raw])))))

(defn cache-import
  "Given an Import, verifies that its hash is good, and saves it to cache"
  [{:keys [hash?] :as import} expression]
  (when hash?
    (let [_type (typecheck expression {})
          normalized (-> expression beta-normalize alpha-normalize)
          bytes (binary/encode normalized)
          actual-hash (sha-256 bytes)]
      (if (= actual-hash hash?)
        (binary/spit-bytes (get-cached-file actual-hash) bytes)
        (fail/hash-mismatch! import actual-hash normalized)))))

(defprotocol IResolve
  (resolve-imports [this state]
    "Takes an expression that might contain import expressions,
    and returns an expression where all imports have been resolved,
    verified and cached.
    Will throw an exception (of the `:dhall-clj.fail/imports` family)
    if some import cannot be resolved."))

(extend-protocol IResolve

  Import
  (resolve-imports [{:keys [hash? mode data] :as this} state]
    (let [data (-> data
                  (canonicalize)
                  (chain (:stack state)))
          this (assoc this :data data)]
          ;; TODO: (check for referentially opaque once we import http)
      (s/with-cache!
        (:cache state)
        this
        (let [;; This `dynamic-expr` might still have imports
              dynamic-expr (s/with-cache!
                             (:cache-raw state)
                             this
                             (expr-from-import data mode hash?))
              resolved-expr (resolve-imports
                              dynamic-expr
                              (update state :stack conj data))
              ;; Cache the expression here
              _ (cache-import this resolved-expr)
              ;; Typecheck with empty context here, as imports cannot
              ;; contain free variables
              _             (typecheck resolved-expr {})
              normalized    (beta-normalize resolved-expr)]
          (if-not hash?
            normalized
            (let [expected-hash (str/lower-case hash?)
                  actual-hash   (-> normalized
                                   alpha-normalize
                                   binary/encode
                                   sha-256)]
              (if (= expected-hash actual-hash)
                ;; TODO: save to local cache here
                normalized
                (fail/hash-mismatch! this actual-hash normalized))))))))

  dhall_clj.ast.ImportAlt
  (resolve-imports [{:keys [a b]} state]
    (ex/try+
      (resolve-imports a state)
      (catch-data :dhall-clj.fail/imports {:as data-a}
        (ex/try+
          (resolve-imports b state)
          (catch-data :dhall-clj.fail/imports {:as data-b}
            (let [typ-a (:type data-a)
                  typ-b (:type data-b)
                  imported (:stack state)]
              (cond
                (and (= typ-a :dhall-clj.fail/missing-imports)
                     (= typ-b :dhall-clj.fail/missing-imports))
                (fail/missing-imports!
                  (concat (:errors data-a) (:errors data-b))
                  imported)

                (= typ-a :dhall-clj.fail/missing-imports)
                (fail/missing-imports!
                  (conj (:errors data-a) data-b)
                  imported)

                (= typ-b :dhall-clj.fail/missing-imports)
                (fail/missing-imports!
                  (conjl (:errors data-b) data-a)
                  imported)

                :else
                (fail/missing-imports!
                  (double-list data-a data-b)
                  imported))))))))


  dhall_clj.ast.Const
  (resolve-imports [this state] this)

  dhall_clj.ast.Var
  (resolve-imports [this state] this)

  dhall_clj.ast.Lam
  (resolve-imports [this state]
    (-> this
       (update :type resolve-imports state)
       (update :body resolve-imports state)))

  dhall_clj.ast.Pi
  (resolve-imports [this state]
    (-> this
       (update :type resolve-imports state)
       (update :body resolve-imports state)))

  dhall_clj.ast.App
  (resolve-imports [this state]
    (-> this
       (update :a resolve-imports state)
       (update :b resolve-imports state)))

  dhall_clj.ast.Let
  (resolve-imports [{:keys [bindings next] :as this} state]
    (assoc
      this
      :bindings (mapv
                  (fn [{:keys [type? e] :as binding}]
                    (assoc
                      binding
                      :type? (when type? (resolve-imports type? state))
                      :e (resolve-imports e state)))
                  bindings)
      :next (resolve-imports next state)))

  dhall_clj.ast.Annot
  (resolve-imports [this state]
    (-> this
       (update :val  resolve-imports state)
       (update :type resolve-imports state)))

  dhall_clj.ast.BoolT
  (resolve-imports [this state] this)

  dhall_clj.ast.BoolLit
  (resolve-imports [this state] this)

  dhall_clj.ast.BoolAnd
  (resolve-imports [this state]
    (-> this
       (update :a resolve-imports state)
       (update :b resolve-imports state)))

  dhall_clj.ast.BoolOr
  (resolve-imports [this state]
    (-> this
       (update :a resolve-imports state)
       (update :b resolve-imports state)))

  dhall_clj.ast.BoolEQ
  (resolve-imports [this state]
    (-> this
       (update :a resolve-imports state)
       (update :b resolve-imports state)))

  dhall_clj.ast.BoolNE
  (resolve-imports [this state]
    (-> this
       (update :a resolve-imports state)
       (update :b resolve-imports state)))

  dhall_clj.ast.BoolIf
  (resolve-imports [this state]
    (-> this
       (update :test resolve-imports state)
       (update :then resolve-imports state)
       (update :else resolve-imports state)))

  dhall_clj.ast.NaturalT
  (resolve-imports [this state] this)

  dhall_clj.ast.NaturalLit
  (resolve-imports [this state] this)

  dhall_clj.ast.NaturalFold
  (resolve-imports [this state] this)

  dhall_clj.ast.NaturalBuild
  (resolve-imports [this state] this)

  dhall_clj.ast.NaturalIsZero
  (resolve-imports [this state] this)

  dhall_clj.ast.NaturalEven
  (resolve-imports [this state] this)

  dhall_clj.ast.NaturalOdd
  (resolve-imports [this state] this)

  dhall_clj.ast.NaturalToInteger
  (resolve-imports [this state] this)

  dhall_clj.ast.NaturalShow
  (resolve-imports [this state] this)

  dhall_clj.ast.NaturalPlus
  (resolve-imports [this state]
    (-> this
       (update :a resolve-imports state)
       (update :b resolve-imports state)))

  dhall_clj.ast.NaturalTimes
  (resolve-imports [this state]
    (-> this
       (update :a resolve-imports state)
       (update :b resolve-imports state)))

  dhall_clj.ast.IntegerT
  (resolve-imports [this state] this)

  dhall_clj.ast.IntegerLit
  (resolve-imports [this state] this)

  dhall_clj.ast.IntegerShow
  (resolve-imports [this state] this)

  dhall_clj.ast.IntegerToDouble
  (resolve-imports [this state] this)

  dhall_clj.ast.DoubleT
  (resolve-imports [this state] this)

  dhall_clj.ast.DoubleLit
  (resolve-imports [this state] this)

  dhall_clj.ast.DoubleShow
  (resolve-imports [this state] this)

  dhall_clj.ast.TextT
  (resolve-imports [this state] this)

  dhall_clj.ast.TextLit
  (resolve-imports [this state]
    (map-chunks this (fn [c] (resolve-imports c state))))

  dhall_clj.ast.TextShow
  (resolve-imports [this state] this)

  dhall_clj.ast.TextAppend
  (resolve-imports [this state]
    (-> this
       (update :a resolve-imports state)
       (update :b resolve-imports state)))

  dhall_clj.ast.ListT
  (resolve-imports [this state] this)

  dhall_clj.ast.ListLit
  (resolve-imports [{:keys [type? exprs] :as this} state]
    (-> this
       (assoc :type? (when type? (resolve-imports type? state))
              :exprs (mapv #(resolve-imports % state) exprs))))

  dhall_clj.ast.ListAppend
  (resolve-imports [this state]
    (-> this
       (update :a resolve-imports state)
       (update :b resolve-imports state)))

  dhall_clj.ast.ListBuild
  (resolve-imports [this state] this)

  dhall_clj.ast.ListFold
  (resolve-imports [this state] this)

  dhall_clj.ast.ListLength
  (resolve-imports [this state] this)

  dhall_clj.ast.ListHead
  (resolve-imports [this state] this)

  dhall_clj.ast.ListLast
  (resolve-imports [this state] this)

  dhall_clj.ast.ListIndexed
  (resolve-imports [this state] this)

  dhall_clj.ast.ListReverse
  (resolve-imports [this state] this)

  dhall_clj.ast.OptionalT
  (resolve-imports [this state] this)

  dhall_clj.ast.OptionalLit
  (resolve-imports [{:keys [val?] :as this} state]
    (-> this
       (update :type resolve-imports state)
       (assoc :val? (when val? (resolve-imports val? state)))))

  dhall_clj.ast.Some
  (resolve-imports [this state]
    (update this :e resolve-imports state))

  dhall_clj.ast.None
  (resolve-imports [this _state] this)

  dhall_clj.ast.OptionalFold
  (resolve-imports [this state] this)

  dhall_clj.ast.OptionalBuild
  (resolve-imports [this state] this)

  dhall_clj.ast.RecordT
  (resolve-imports [this state]
    (update this :kvs (fn [kvs] (map-vals #(resolve-imports % state) kvs))))

  dhall_clj.ast.RecordLit
  (resolve-imports [this state]
    (update this :kvs (fn [kvs] (map-vals #(resolve-imports % state) kvs))))

  dhall_clj.ast.UnionT
  (resolve-imports [this state]
    (update this :kvs (fn [kvs] (map-vals #(when % (resolve-imports % state)) kvs))))

  dhall_clj.ast.UnionLit
  (resolve-imports [{:keys [v?] :as this} state]
    (-> this
       (assoc :v? (when v? (resolve-imports v? state)))
       (update :kvs (fn [kvs] (map-vals #(when % (resolve-imports % state)) kvs)))))

  dhall_clj.ast.Combine
  (resolve-imports [this state]
    (-> this
       (update :a resolve-imports state)
       (update :b resolve-imports state)))

  dhall_clj.ast.CombineTypes
  (resolve-imports [this state]
    (-> this
       (update :a resolve-imports state)
       (update :b resolve-imports state)))

  dhall_clj.ast.Prefer
  (resolve-imports [this state]
    (-> this
       (update :a resolve-imports state)
       (update :b resolve-imports state)))

  dhall_clj.ast.Merge
  (resolve-imports [{:keys [type?] :as this} state]
    (-> this
       (update :a resolve-imports state)
       (update :b resolve-imports state)
       (assoc :type? (when type? (resolve-imports type? state)))))

  dhall_clj.ast.Field
  (resolve-imports [this state]
    (update this :e resolve-imports state))

  dhall_clj.ast.Project
  (resolve-imports [this state]
    (update this :e resolve-imports state)))
