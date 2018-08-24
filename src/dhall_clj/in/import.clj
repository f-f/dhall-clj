(ns dhall-clj.in.import
  (:require [dhall-clj.ast :refer :all]
            [medley.core :refer [map-vals]]
            [digest :refer [sha-256]]
            [dhall-clj.in.parse :refer [parse expr]]
            [dhall-clj.state :as state]
            [dhall-clj.in.fail :as fail]))


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
  dhall_clj.ast.Local
  (canonicalize [this]
    (update this :directory canonicalize-dir)))


;;
;; Resolving imports
;;

(defn expr-from-import
  "Given a Missing, Env, Local or Remote,
  fetches them from the appropriate place."
  [import-data mode]
  ;; Here we pass a `nil` in the State, because we don't
  ;; have (and don't need) it
  (let [raw (resolve-imports import-data nil)]
    (if (= mode :code)
      (-> raw parse expr)
      (->TextLit [raw]))))

(defprotocol IResolve
  (resolve-imports [this state]
    "`resolve-imports` will fetch, verify and cache imports embedded in
    the Expression.
    Will throw an exception (of the `:dhall-clj.in.fail/imports` family)
    in case it cannot resolve some expression."))


(extend-protocol IResolve
  dhall_clj.ast.Missing
  (resolve-imports [_this _state]
    (fail/missing-keyword!))

  dhall_clj.ast.Env
  (resolve-imports [{:keys [name]} _state]
    (if-let [env (System/getenv name)]
      env
      (fail/missing-env! name)))

  dhall_clj.ast.Local
  (resolve-imports [this state]
    this) ;; TODO

  dhall_clj.ast.Import
  (resolve-imports [{:keys [type hash? mode data]} state]
    (let []
      ;; TODO: canonicalize + chain the import
      ;; TODO: (check for referentially opaque once we import http)
      ;; TODO: check the cache, if not there:
      ;; TODO: run expr-from-import on the current import
      ;; TODO: add import to the stack and recur
      ;; TODO: typecheck + normalize
      ;; TODO: save the result in cache
      data))

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
