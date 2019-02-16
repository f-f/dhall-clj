(ns dhall-clj.alpha-normalize
  (:require [dhall-clj.ast :refer :all]
            [medley.core :refer [map-vals]]))


(defprotocol IAlphaNormalize
  (alpha-normalize [this]
    "α-normalize an expression by renaming all variables to `_` and using
    De Bruijn indices to distinguish them"))


(extend-protocol IAlphaNormalize

  dhall_clj.ast.Const
  (alpha-normalize [this] this)

  dhall_clj.ast.Var
  (alpha-normalize [this] this)

  dhall_clj.ast.Lam
  (alpha-normalize [{:keys [arg type body] :as this}]
    (let [var   (->Var arg 0)
          v'    (->Var "_" 0)
          body' (if (= arg "_")
                  (alpha-normalize body)
                  (-> body
                     (shift 1 v')
                     (subst var v')
                     (shift -1 var)
                     (alpha-normalize)))]
      (assoc this
             :arg  "_"
             :type (alpha-normalize type)
             :body body')))

  dhall_clj.ast.Pi
  (alpha-normalize [{:keys [arg type body] :as this}]
    (let [var   (->Var arg 0)
          v'    (->Var "_" 0)
          body' (if (= arg "_")
                  (alpha-normalize body)
                  (-> body
                     (shift 1 v')
                     (subst var v')
                     (shift -1 var)
                     (alpha-normalize)))]
      (assoc this
             :arg  "_"
             :type (alpha-normalize type)
             :body body')))

  dhall_clj.ast.App
  (alpha-normalize [this]
    (-> this
       (update :a alpha-normalize)
       (update :b alpha-normalize)))

  dhall_clj.ast.Let
  (alpha-normalize [{:keys [bindings next] :as this}]
    (let [[{:keys [label type? e] :as binding} & more-bindings] bindings
          var   (->Var label 0)
          v'    (->Var "_" 0)
          next' (if (= label "_")
                  (alpha-normalize next)
                  (-> next
                     (shift 1 v')
                     (subst var v')
                     (shift -1 var)
                     (alpha-normalize)))
          type?'   (when type? (alpha-normalize type?))
          binding' (assoc
                     binding
                     :label "_"
                     :type? type?'
                     :e (alpha-normalize e))]
      (if-not (seq more-bindings)
        (assoc
          this
          :bindings (list binding')
          :next next')
        (-> this
           (update :bindings rest)
           (assoc :next next')
           (alpha-normalize)
           (update :bindings conj binding')))))

  dhall_clj.ast.Annot
  (alpha-normalize [this]
    (-> this
       (update :val  alpha-normalize)
       (update :type alpha-normalize)))

  dhall_clj.ast.BoolT
  (alpha-normalize [this] this)

  dhall_clj.ast.BoolLit
  (alpha-normalize [this] this)

  dhall_clj.ast.BoolAnd
  (alpha-normalize [this]
    (-> this
       (update :a alpha-normalize)
       (update :b alpha-normalize)))

  dhall_clj.ast.BoolOr
  (alpha-normalize [this]
    (-> this
       (update :a alpha-normalize)
       (update :b alpha-normalize)))

  dhall_clj.ast.BoolEQ
  (alpha-normalize [this]
    (-> this
       (update :a alpha-normalize)
       (update :b alpha-normalize)))

  dhall_clj.ast.BoolNE
  (alpha-normalize [this]
    (-> this
       (update :a alpha-normalize)
       (update :b alpha-normalize)))

  dhall_clj.ast.BoolIf
  (alpha-normalize [this]
    (-> this
       (update :test alpha-normalize)
       (update :then alpha-normalize)
       (update :else alpha-normalize)))

  dhall_clj.ast.NaturalT
  (alpha-normalize [this] this)

  dhall_clj.ast.NaturalLit
  (alpha-normalize [this] this)

  dhall_clj.ast.NaturalFold
  (alpha-normalize [this] this)

  dhall_clj.ast.NaturalBuild
  (alpha-normalize [this] this)

  dhall_clj.ast.NaturalIsZero
  (alpha-normalize [this] this)

  dhall_clj.ast.NaturalEven
  (alpha-normalize [this] this)

  dhall_clj.ast.NaturalOdd
  (alpha-normalize [this] this)

  dhall_clj.ast.NaturalToInteger
  (alpha-normalize [this] this)

  dhall_clj.ast.NaturalShow
  (alpha-normalize [this] this)

  dhall_clj.ast.NaturalPlus
  (alpha-normalize [this]
    (-> this
       (update :a alpha-normalize)
       (update :b alpha-normalize)))

  dhall_clj.ast.NaturalTimes
  (alpha-normalize [this]
    (-> this
       (update :a alpha-normalize)
       (update :b alpha-normalize)))

  dhall_clj.ast.IntegerT
  (alpha-normalize [this] this)

  dhall_clj.ast.IntegerLit
  (alpha-normalize [this] this)

  dhall_clj.ast.IntegerShow
  (alpha-normalize [this] this)

  dhall_clj.ast.IntegerToDouble
  (alpha-normalize [this] this)

  dhall_clj.ast.DoubleT
  (alpha-normalize [this] this)

  dhall_clj.ast.DoubleLit
  (alpha-normalize [this] this)

  dhall_clj.ast.DoubleShow
  (alpha-normalize [this] this)

  dhall_clj.ast.TextT
  (alpha-normalize [this] this)

  dhall_clj.ast.TextLit
  (alpha-normalize [this]
    (map-chunks this alpha-normalize))

  dhall_clj.ast.TextShow
  (alpha-normalize [this] this)

  dhall_clj.ast.TextAppend
  (alpha-normalize [this]
    (-> this
       (update :a alpha-normalize)
       (update :b alpha-normalize)))

  dhall_clj.ast.ListT
  (alpha-normalize [this] this)

  dhall_clj.ast.ListLit
  (alpha-normalize [{:keys [type?] :as this}]
    (-> this
       (assoc :type?  (when type? (alpha-normalize type?)))
       (update :exprs (partial map alpha-normalize))))

  dhall_clj.ast.ListAppend
  (alpha-normalize [this]
    (-> this
       (update :a alpha-normalize)
       (update :b alpha-normalize)))

  dhall_clj.ast.ListBuild
  (alpha-normalize [this] this)

  dhall_clj.ast.ListFold
  (alpha-normalize [this] this)

  dhall_clj.ast.ListLength
  (alpha-normalize [this] this)

  dhall_clj.ast.ListHead
  (alpha-normalize [this] this)

  dhall_clj.ast.ListLast
  (alpha-normalize [this] this)

  dhall_clj.ast.ListIndexed
  (alpha-normalize [this] this)

  dhall_clj.ast.ListReverse
  (alpha-normalize [this] this)

  dhall_clj.ast.OptionalT
  (alpha-normalize [this] this)

  dhall_clj.ast.OptionalLit
  (alpha-normalize [{:keys [val?] :as this}]
    (-> this
       (update :type alpha-normalize)
       (assoc  :val? (when val? (alpha-normalize val?)))))

  dhall_clj.ast.Some
  (alpha-normalize [this]
    (update this :e alpha-normalize))

  dhall_clj.ast.None
  (alpha-normalize [this] this)

  dhall_clj.ast.OptionalFold
  (alpha-normalize [this] this)

  dhall_clj.ast.OptionalBuild
  (alpha-normalize [this] this)

  dhall_clj.ast.RecordT
  (alpha-normalize [this]
    (update this :kvs (fn [kvs] (map-vals alpha-normalize kvs))))

  dhall_clj.ast.RecordLit
  (alpha-normalize [this]
    (update this :kvs (fn [kvs] (map-vals alpha-normalize kvs))))

  dhall_clj.ast.UnionT
  (alpha-normalize [this]
    (update this :kvs (fn [kvs] (map-vals alpha-normalize kvs))))

  dhall_clj.ast.UnionLit
  (alpha-normalize [this]
    (-> this
       (update :v alpha-normalize)
       (update :kvs (fn [kvs] (map-vals alpha-normalize kvs)))))

  dhall_clj.ast.Combine
  (alpha-normalize [this]
    (-> this
       (update :a alpha-normalize)
       (update :b alpha-normalize)))

  dhall_clj.ast.CombineTypes
  (alpha-normalize [this]
    (-> this
       (update :a alpha-normalize)
       (update :b alpha-normalize)))

  dhall_clj.ast.Prefer
  (alpha-normalize [this]
    (-> this
       (update :a alpha-normalize)
       (update :b alpha-normalize)))

  dhall_clj.ast.Merge
  (alpha-normalize [{:keys [type?] :as this}]
    (-> this
       (update :a alpha-normalize)
       (update :b alpha-normalize)
       (assoc :type? (when type? (alpha-normalize type?)))))

  dhall_clj.ast.Constructors
  (alpha-normalize [this]
    (update this :e alpha-normalize))

  dhall_clj.ast.Field
  (alpha-normalize [this]
    (update this :e alpha-normalize))

  dhall_clj.ast.Project
  (alpha-normalize [this]
    (update this :e alpha-normalize))

  dhall_clj.ast.ImportAlt
  (alpha-normalize [this]
    (-> this
       (update :a alpha-normalize)
       (update :b alpha-normalize))))
