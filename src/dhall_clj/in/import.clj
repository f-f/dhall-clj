(ns dhall-clj.in.import
  (:require [dhall-clj.ast :refer :all]
            [medley.core :refer [map-vals]]
            [digest :refer [sha-256]]
            [dhall-clj.in.fail :as fail]))


;; Imports data structures

(defrecord Import [type   ;; :local, :remote, :env, :missing
                   hash?  ;; maybe a sha256 in hex
                   mode   ;; :code or :text
                   data]) ;; the actual import, records defined below

(defrecord Local [prefix directory file])
(defrecord Remote [url headers?])
(defrecord Env [name])
(defrecord Missing [])


;; Cache

(defn new-cache
  "Creates a new cache atom.
  The `:raw` cache has the path of the import as key, and the raw text
  of the expression as value.
  The `:ast` cache has the hashed raw-text as key, and the resolved
  and normalized AST as value, so we don't have to re-evaluate things."
  []
  (atom {:import {}
         :ast    {}}))

(defmacro with-cache
  "Given a cache, a key vector and a body to compute it its value,
  returns the cached result or performs the computation and caches it.
  Do not use directly, see `with-import-cache` and `with-ast-cache`"
  [cache path body]
  (let [cache' cache]
    `(let [new# (swap! ~cache'
                       (fn [c#]
                         (if (get-in c# ~path)
                           c#
                           (assoc-in c# ~path ~body))))]
       (get-in new# ~path))))

(defmacro with-import-cache
  "Given a cache, an import name, and a body to compute it,
  performs the side-effecting body to resolve the import,
  or returns the cached if already resolved."
  [cache path body]
  `(with-cache ~cache [:import ~path] ~body))


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

(defprotocol ICanonicalize
  (canonicalize [this]
    "`canonicalize` will normalize the path position of directories"))

(extend-protocol ICanonicalize
  Local
  (canonicalize [this]
    (update this :directory canonicalize-dir)))


;;
;; Resolving imports
;;

(defprotocol IResolve
  (resolve-imports [this cache]
    "`resolve-imports` will fetch, verify and cache imports embedded in
    the Expression.
    Will throw an exception (of the `:dhall-clj.in.fail/imports` family)
    in case it cannot resolve some expression."))


(extend-protocol IResolve
  Missing
  (resolve-imports [_this cache]
    (fail/missing-keyword!))

  Env
  (resolve-imports [{:keys [name]} cache]
    (with-cache
      cache
      name
      (if-let [env (System/getenv name)]
        env
        (fail/missing-env! name))))

  Import
  (resolve-imports [{:keys [type hash? mode data]} cache]
    (let [raw (resolve-imports data cache)
          ;; TODO check + cache hashed stuff
          ast (if (= mode :code)
                (-> raw parse expr)
                (->TextLit [raw]))]
          ;; TODO recursively resolve imports
          ;; TODO check for cycles
          ;; TODO check for referentiallyopaque once we import http
          ;; TODO typecheck + normalize
      ast))

  dhall_clj.ast.ImportAlt
  (resolve-imports [this cache]
    this) ;; TODO

  dhall_clj.ast.Const
  (resolve-imports [this cache] this)

  dhall_clj.ast.Var
  (resolve-imports [this cache] this)

  dhall_clj.ast.Lam
  (resolve-imports [this cache]
    (-> this
       (update :type resolve-imports cache)
       (update :body resolve-imports cache)))

  dhall_clj.ast.Pi
  (resolve-imports [this cache]
    (-> this
       (update :type resolve-imports cache)
       (update :body resolve-imports cache)))

  dhall_clj.ast.App
  (resolve-imports [this cache]
    (-> this
       (update :a resolve-imports cache)
       (update :b resolve-imports cache)))

  dhall_clj.ast.Let
  (resolve-imports [{:keys [type?] :as this} cache]
    (-> this
       (assoc  :type? (when type? (resolve-imports type? cache)))
       (update :body  resolve-imports cache)
       (update :next  resolve-imports cache)))

  dhall_clj.ast.Annot
  (resolve-imports [this cache]
    (-> this
       (update :val  resolve-imports cache)
       (update :type resolve-imports cache)))

  dhall_clj.ast.BoolT
  (resolve-imports [this cache] this)

  dhall_clj.ast.BoolLit
  (resolve-imports [this cache] this)

  dhall_clj.ast.BoolAnd
  (resolve-imports [this cache]
    (-> this
       (update :a resolve-imports cache)
       (update :b resolve-imports cache)))

  dhall_clj.ast.BoolOr
  (resolve-imports [this cache]
    (-> this
       (update :a resolve-imports cache)
       (update :b resolve-imports cache)))

  dhall_clj.ast.BoolEQ
  (resolve-imports [this cache]
    (-> this
       (update :a resolve-imports cache)
       (update :b resolve-imports cache)))

  dhall_clj.ast.BoolNE
  (resolve-imports [this cache]
    (-> this
       (update :a resolve-imports cache)
       (update :b resolve-imports cache)))

  dhall_clj.ast.BoolIf
  (resolve-imports [this cache]
    (-> this
       (update :test resolve-imports cache)
       (update :then resolve-imports cache)
       (update :else resolve-imports cache)))

  dhall_clj.ast.NaturalT
  (resolve-imports [this cache] this)

  dhall_clj.ast.NaturalLit
  (resolve-imports [this cache] this)

  dhall_clj.ast.NaturalFold
  (resolve-imports [this cache] this)

  dhall_clj.ast.NaturalBuild
  (resolve-imports [this cache] this)

  dhall_clj.ast.NaturalIsZero
  (resolve-imports [this cache] this)

  dhall_clj.ast.NaturalEven
  (resolve-imports [this cache] this)

  dhall_clj.ast.NaturalOdd
  (resolve-imports [this cache] this)

  dhall_clj.ast.NaturalToInteger
  (resolve-imports [this cache] this)

  dhall_clj.ast.NaturalShow
  (resolve-imports [this cache] this)

  dhall_clj.ast.NaturalPlus
  (resolve-imports [this cache]
    (-> this
       (update :a resolve-imports cache)
       (update :b resolve-imports cache)))

  dhall_clj.ast.NaturalTimes
  (resolve-imports [this cache]
    (-> this
       (update :a resolve-imports cache)
       (update :b resolve-imports cache)))

  dhall_clj.ast.IntegerT
  (resolve-imports [this cache] this)

  dhall_clj.ast.IntegerLit
  (resolve-imports [this cache] this)

  dhall_clj.ast.IntegerShow
  (resolve-imports [this cache] this)

  dhall_clj.ast.IntegerToDouble
  (resolve-imports [this cache] this)

  dhall_clj.ast.DoubleT
  (resolve-imports [this cache] this)

  dhall_clj.ast.DoubleLit
  (resolve-imports [this cache] this)

  dhall_clj.ast.DoubleShow
  (resolve-imports [this cache] this)

  dhall_clj.ast.TextT
  (resolve-imports [this cache] this)

  dhall_clj.ast.TextLit
  (resolve-imports [this cache]
    (map-chunks this (fn [c] (resolve-imports c cache))))

  dhall_clj.ast.TextAppend
  (resolve-imports [this cache]
    (-> this
       (update :a resolve-imports cache)
       (update :b resolve-imports cache)))

  dhall_clj.ast.ListT
  (resolve-imports [this cache] this)

  dhall_clj.ast.ListLit
  (resolve-imports [{:keys [type? exprs] :as this} cache]
    (-> this
       (assoc :type? (when type? (resolve-imports type? cache))
              :exprs (mapv #(resolve-imports % cache) exprs))))

  dhall_clj.ast.ListAppend
  (resolve-imports [this cache]
    (-> this
       (update :a resolve-imports cache)
       (update :b resolve-imports cache)))

  dhall_clj.ast.ListBuild
  (resolve-imports [this cache] this)

  dhall_clj.ast.ListFold
  (resolve-imports [this cache] this)

  dhall_clj.ast.ListLength
  (resolve-imports [this cache] this)

  dhall_clj.ast.ListHead
  (resolve-imports [this cache] this)

  dhall_clj.ast.ListLast
  (resolve-imports [this cache] this)

  dhall_clj.ast.ListIndexed
  (resolve-imports [this cache] this)

  dhall_clj.ast.ListReverse
  (resolve-imports [this cache] this)

  dhall_clj.ast.OptionalT
  (resolve-imports [this cache] this)

  dhall_clj.ast.OptionalLit
  (resolve-imports [{:keys [val?] :as this} cache]
    (-> this
       (update :type resolve-imports cache)
       (assoc :val? (when val? (resolve-imports val? cache)))))

  dhall_clj.ast.OptionalFold
  (resolve-imports [this cache] this)

  dhall_clj.ast.OptionalBuild
  (resolve-imports [this cache] this)

  dhall_clj.ast.RecordT
  (resolve-imports [this cache]
    (update this :kvs (fn [kvs] (map-vals #(resolve-imports % cache) kvs))))

  dhall_clj.ast.RecordLit
  (resolve-imports [this cache]
    (update this :kvs (fn [kvs] (map-vals #(resolve-imports % cache) kvs))))

  dhall_clj.ast.UnionT
  (resolve-imports [this cache]
    (update this :kvs (fn [kvs] (map-vals #(resolve-imports % cache) kvs))))

  dhall_clj.ast.UnionLit
  (resolve-imports [this cache]
    (-> this
       (update :v resolve-imports cache)
       (update :kvs (fn [kvs] (map-vals #(resolve-imports % cache) kvs)))))

  dhall_clj.ast.Combine
  (resolve-imports [this cache]
    (-> this
       (update :a resolve-imports cache)
       (update :b resolve-imports cache)))

  dhall_clj.ast.CombineTypes
  (resolve-imports [this cache]
    (-> this
       (update :a resolve-imports cache)
       (update :b resolve-imports cache)))

  dhall_clj.ast.Prefer
  (resolve-imports [this cache]
    (-> this
       (update :a resolve-imports cache)
       (update :b resolve-imports cache)))

  dhall_clj.ast.Constructors
  (resolve-imports [this cache]
    (update this :e resolve-imports cache))

  dhall_clj.ast.Merge
  (resolve-imports [{:keys [type?] :as this} cache]
    (-> this
       (update :a resolve-imports cache)
       (update :b resolve-imports cache)
       (assoc :type? (when type? (resolve-imports type? cache)))))

  dhall_clj.ast.Field
  (resolve-imports [this cache]
    (update this :e resolve-imports cache))

  dhall_clj.ast.Project
  (resolve-imports [this cache]
    (update this :e resolve-imports cache)))
