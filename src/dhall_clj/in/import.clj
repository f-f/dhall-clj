(ns dhall-clj.in.import
  (:require [dhall-clj.ast :refer :all]
            [medley.core :refer [map-vals]]))


;; Imports data structures

(defrecord Import [type   ;; :local, :remote, :env, :missing
                   hash?  ;; maybe a sha256 in hex
                   mode   ;; :code or :text
                   data]) ;; the actual import, records defined below

(defrecord Local [prefix directory file])
(defrecord Remote [url headers?])
(defrecord Env [name])
(defrecord Missing [])


(defn error [context data]
  (do (println (str "Failed match: " context))
      data))

(defn new-cache
  []
  (atom {:env  {}
         :http {}
         :path {}}))

(defn cache-path [cache path]
  (let [new (swap!
              cache
              (fn [c p]
                (if (get-in c [:path path])
                  c
                  (assoc-in c [:path path] path))) ;;(slurp path))))
              path)]
    (get-in new [:path path])))




(defprotocol IResolve
  (resolve-imports [this cache]
    "`resolve-imports` will fetch, verify and cache imports embedded in
    the Expression.
    Will throw an exception (of the `:dhall-clojure.in.fail/parse` family)
    in case it cannot resolve some expression."))


(extend-protocol IResolve
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
