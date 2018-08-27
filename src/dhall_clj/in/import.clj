(ns dhall-clj.in.import
  (:require [dhall-clj.ast :refer :all]
            [medley.core :refer [map-vals]]
            [digest :refer [sha-256]]
            [me.raynes.fs :as fs]
            [dhall-clj.in.parse :refer [parse expr]]
            [dhall-clj.state :as state]
            [dhall-clj.in.fail :as fail]
            [clojure.string :as str]
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
                   ".." (str fs/*cwd* "/..")
                   nil)
          filepath (str prefix
                        "/"
                        (str/join "/" (reverse directory))
                        "/"
                        file)]
      (if (fs/exists? filepath)
        (slurp filepath)
        (fail/missing-file! filepath this)))))

;;
;; Resolving imports
;;

(defn expr-from-import
  "Given a Missing, Env, Local or Remote, fetches them
  from the appropriate place, and returns and expression
  (that might contain more imports)."
  [import-data mode]
  (let [raw (fetch import-data)]
    (if (= mode :code)
      (-> raw parse expr)
      (->TextLit [raw]))))


(defprotocol IResolve
  (resolve-imports [this state]
    "Takes an expression that might contain import expressions,
    and returns an expression where all imports have been resolved,
    verified and cached.
    Will throw an exception (of the `:dhall-clj.in.fail/imports` family)
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
                             (expr-from-import data mode))
              resolved-expr (resolve-imports
                              dynamic-expr
                              (update state :stack conj data))]
          ;; TODO: typecheck + normalize
          resolved-expr))))

  dhall_clj.ast.ImportAlt
  (resolve-imports [this state]
    this) ;; TODO

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
  (resolve-imports [{:keys [type?] :as this} state]
    (-> this
       (assoc  :type? (when type? (resolve-imports type? state)))
       (update :body  resolve-imports state)
       (update :next  resolve-imports state)))

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
    (update this :kvs (fn [kvs] (map-vals #(resolve-imports % state) kvs))))

  dhall_clj.ast.UnionLit
  (resolve-imports [this state]
    (-> this
       (update :v resolve-imports state)
       (update :kvs (fn [kvs] (map-vals #(resolve-imports % state) kvs)))))

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

  dhall_clj.ast.Constructors
  (resolve-imports [this state]
    (update this :e resolve-imports state))

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
