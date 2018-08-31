(ns dhall-clj.typecheck
  (:require [dhall-clj.ast :refer :all]
            [dhall-clj.context :as context]
            [dhall-clj.in.fail :as fail]
            [dhall-clj.beta-normalize :refer [beta-normalize judgmentally-equal]])
  (:import [dhall_clj.ast BoolT NaturalT TextT App ListT]))


(defmacro typecheck-binary
  "Generates the typechecking code for a binary operation given the
  return `typ-class` and the `exc-fn` to throw in case of type error."
  [this ctx typ-class exc-fn]
  (let [op-constructor (symbol (str "->" typ-class))]
    `(let [a#  (:a ~this)
           b#  (:b ~this)
           aT# (-> a# (typecheck ~ctx) beta-normalize)
           bT# (-> b# (typecheck ~ctx) beta-normalize)]
       (cond
         (not (instance? ~typ-class aT#)) (~exc-fn ~ctx ~this {:a a# :a-type aT#})
         (not (instance? ~typ-class bT#)) (~exc-fn ~ctx ~this {:b b# :b-type bT#})
         :else                      (~op-constructor)))))


(defprotocol ITypecheck
  (typecheck [this context]
    "Typecheck an expression in a context.
    Returns the type on success, or excepts."))


(extend-protocol ITypecheck

  dhall_clj.ast.Const
  (typecheck [{:keys [c] :as this} ctx]
    (if (= :type c)
      (assoc this :c :kind)
      (fail/untyped! ctx this)))

  dhall_clj.ast.Var
  (typecheck [{:keys [x i] :as this} ctx]
    (if-let [val (context/lookup ctx x i)]
      val
      (fail/unbound-variable! ctx this {:name x})))

  dhall_clj.ast.Lam
  (typecheck [{:keys [arg type body] :as this} ctx]
    (let [_    (typecheck type ctx)
          ctx' (-> (beta-normalize type)
                  (context/insert arg ctx)
                  (context/transform (fn [e] (shift e 1 (->Var arg 0)))))
          body-typ (typecheck body ctx')
          pi       (->Pi arg type body-typ)
          _        (typecheck pi ctx)]
      pi))

  dhall_clj.ast.Pi
  (typecheck [this ctx] "TODO typecheck Pi")

  dhall_clj.ast.App
  (typecheck [this ctx] "TODO typecheck App")

  dhall_clj.ast.Let
  (typecheck [this ctx] "TODO typecheck Let")

  dhall_clj.ast.Annot
  (typecheck [{:keys [val type] :as this} ctx]
    (let [_     (typecheck type ctx)
          type' (typecheck val  ctx)]
      (if (judgmentally-equal type type')
        type
        (fail/annot-mismatch! ctx this {:val  (beta-normalize type')
                                        :type (beta-normalize type)}))))

  dhall_clj.ast.BoolT
  (typecheck [this _ctx]
    (->Const :type))

  dhall_clj.ast.BoolLit
  (typecheck [this _ctx]
    (->BoolT))

  dhall_clj.ast.BoolAnd
  (typecheck [{:keys [a b] :as this} ctx]
    (let [aT (-> a (typecheck ctx) beta-normalize)
          bT (-> b (typecheck ctx) beta-normalize)]
      (cond
        (not (instance? BoolT aT)) (fail/cant-and! ctx this {:a a :a-type aT})
        (not (instance? BoolT bT)) (fail/cant-and! ctx this {:b b :b-type bT})
        :else                      (->BoolT))))

  dhall_clj.ast.BoolOr
  (typecheck [{:keys [a b] :as this} ctx]
    (let [aT (-> a (typecheck ctx) beta-normalize)
          bT (-> b (typecheck ctx) beta-normalize)]
      (cond
        (not (instance? BoolT aT)) (fail/cant-or! ctx this {:a a :a-type aT})
        (not (instance? BoolT bT)) (fail/cant-or! ctx this {:b b :b-type bT})
        :else                      (->BoolT))))

  dhall_clj.ast.BoolEQ
  (typecheck [{:keys [a b] :as this} ctx]
    (let [aT (-> a (typecheck ctx) beta-normalize)
          bT (-> b (typecheck ctx) beta-normalize)]
      (cond
        (not (instance? BoolT aT)) (fail/cant-eq! ctx this {:a a :a-type aT})
        (not (instance? BoolT bT)) (fail/cant-eq! ctx this {:b b :b-type bT})
        :else                      (->BoolT))))

  dhall_clj.ast.BoolNE
  (typecheck [{:keys [a b] :as this} ctx]
    (let [aT (-> a (typecheck ctx) beta-normalize)
          bT (-> b (typecheck ctx) beta-normalize)]
      (cond
        (not (instance? BoolT aT)) (fail/cant-neq! ctx this {:a a :a-type aT})
        (not (instance? BoolT bT)) (fail/cant-neq! ctx this {:b b :b-type bT})
        :else                      (->BoolT))))

  dhall_clj.ast.BoolIf
  (typecheck [{:keys [test then else] :as this} ctx]
    (let [testT  (-> test (typecheck ctx) beta-normalize)
          _      (when (not (instance? BoolT testT))
                   (fail/invalid-predicate!
                     ctx
                     this
                     {:test test :testT testT}))
          thenT  (-> then  (typecheck ctx) beta-normalize)
          thenTT (-> thenT (typecheck ctx) beta-normalize)
          _      (when (not= thenTT (->Const :type))
                   (fail/if-branch-must-be-term!
                     ctx
                     this
                     {:then then :thenT thenT :thenTT thenTT}))
          elseT  (-> else  (typecheck ctx) beta-normalize)
          elseTT (-> elseT (typecheck ctx) beta-normalize)
          _      (when (not= elseTT (->Const :type))
                   (fail/if-branch-must-be-term!
                     ctx
                     this
                     {:else else :elseT elseT :elseTT elseTT}))]
      (if (judgmentally-equal thenT elseT)
        thenT
        (fail/if-branch-mismatch!
          ctx
          this
          {:then then
           :else else
           :thenT thenT
           :elseT elseT}))))

  dhall_clj.ast.NaturalT
  (typecheck [this _ctx]
    (->Const :type))

  dhall_clj.ast.NaturalLit
  (typecheck [this ctx]
    (->NaturalT))

  dhall_clj.ast.NaturalFold
  (typecheck [this _ctx]
    (let [natural (->Var "natural" 0)]
      (->Pi "_"
            (->NaturalT)
            (->Pi "natural"
                  (->Const :type)
                  (->Pi "succ"
                        (->Pi "_" natural natural)
                        (->Pi "zero" natural natural))))))

  dhall_clj.ast.NaturalBuild
  (typecheck [this _ctx]
    (let [natural (->Var "natural" 0)]
      (->Pi "_"
            (->Pi "natural"
                  (->Const :type)
                  (->Pi "succ"
                        (->Pi "_" natural natural)
                        (->Pi "zero" natural natural)))
            (->NaturalT))))

  dhall_clj.ast.NaturalIsZero
  (typecheck [this _ctx]
    (->Pi "_" (->NaturalT) (->BoolT)))

  dhall_clj.ast.NaturalEven
  (typecheck [this _ctx]
    (->Pi "_" (->NaturalT) (->BoolT)))

  dhall_clj.ast.NaturalOdd
  (typecheck [this _ctx]
    (->Pi "_" (->NaturalT) (->BoolT)))

  dhall_clj.ast.NaturalToInteger
  (typecheck [this _ctx]
    (->Pi "_" (->NaturalT) (->IntegerT)))

  dhall_clj.ast.NaturalShow
  (typecheck [this _ctx]
    (->Pi "_" (->NaturalT) (->TextT)))

  dhall_clj.ast.NaturalPlus
  (typecheck [{:keys [a b] :as this} ctx]
    (let [aT (-> a (typecheck ctx) beta-normalize)
          bT (-> b (typecheck ctx) beta-normalize)]
      (cond
        (not (instance? NaturalT aT)) (fail/cant-add! ctx this {:a a :a-type aT})
        (not (instance? NaturalT bT)) (fail/cant-add! ctx this {:b b :b-type bT})
        :else                         (->NaturalT))))


  dhall_clj.ast.NaturalTimes
  (typecheck [{:keys [a b] :as this} ctx]
    (let [aT (-> a (typecheck ctx) beta-normalize)
          bT (-> b (typecheck ctx) beta-normalize)]
      (cond
        (not (instance? NaturalT aT)) (fail/cant-multiply! ctx this {:a a :a-type aT})
        (not (instance? NaturalT bT)) (fail/cant-multiply! ctx this {:b b :b-type bT})
        :else                         (->NaturalT))))


  dhall_clj.ast.IntegerT
  (typecheck [this _ctx]
    (->Const :type))

  dhall_clj.ast.IntegerLit
  (typecheck [this _ctx]
    (->IntegerT))

  dhall_clj.ast.IntegerShow
  (typecheck [this _ctx]
    (->Pi "_" (->IntegerT) (->TextT)))

  dhall_clj.ast.IntegerToDouble
  (typecheck [this _ctx]
    (->Pi "_" (->IntegerT) (->DoubleT)))

  dhall_clj.ast.DoubleT
  (typecheck [this _ctx]
    (->Const :type))

  dhall_clj.ast.DoubleLit
  (typecheck [this _ctx]
    (->DoubleT))

  dhall_clj.ast.DoubleShow
  (typecheck [this _ctx]
    (->Pi "_" (->DoubleT) (->TextT)))

  dhall_clj.ast.TextT
  (typecheck [this _ctx]
    (->Const :type))

  dhall_clj.ast.TextLit
  (typecheck [this ctx] "TODO typecheck TextLit")

  dhall_clj.ast.TextAppend
  (typecheck [{:keys [a b] :as this} ctx]
    (let [aT (-> a (typecheck ctx) beta-normalize)
          bT (-> b (typecheck ctx) beta-normalize)]
      (cond
        (not (instance? TextT aT)) (fail/cant-text-append! ctx this {:a a :a-type aT})
        (not (instance? TextT bT)) (fail/cant-text-append! ctx this {:b b :b-type bT})
        :else                      (->TextT))))

  dhall_clj.ast.ListT
  (typecheck [this _ctx]
    (->Pi "_" (->Const :type) (->Const :type)))

  dhall_clj.ast.ListLit
  (typecheck [this ctx] "TODO typecheck ListLit")

  dhall_clj.ast.ListAppend
  (typecheck [{:keys [a b] :as this} ctx]
    (let [aT (-> a (typecheck ctx) beta-normalize)
          bT (-> b (typecheck ctx) beta-normalize)
          a-elemT (if (and (instance? App aT) (instance? ListT (:a aT)))
                    (:b aT)
                    (fail/cant-list-append! ctx this {:a a :a-type aT}))
          b-elemT (if (and (instance? App bT) (instance? ListT (:a bT)))
                    (:b bT)
                    (fail/cant-list-append! ctx this {:b b :b-type bT}))]
      (if (judgmentally-equal a-elemT b-elemT)
        aT
        (fail/list-append-mismatch! ctx this {:elem-a-type a-elemT :elem-b-type b-elemT}))))

  dhall_clj.ast.ListBuild
  (typecheck [this _ctx]
    (let [list (->Var "list" 0)
          a    (->Var "a" 0)]
      (->Pi "a"
            (->Const :type)
            (->Pi "_"
                  (->Pi "list"
                        (->Const :type)
                        (->Pi "cons"
                              (->Pi "_" a (->Pi "_" list list))
                              (->Pi "nil" list list)))
                  (->App (->ListT) a)))))

  dhall_clj.ast.ListFold
  (typecheck [this _ctx]
    (let [list (->Var "list" 0)
          a    (->Var "a" 0)]
      (->Pi "a"
            (->Const :type)
            (->Pi "_"
                  (->App (->ListT) a)
                  (->Pi "list"
                        (->Const :type)
                        (->Pi "cons"
                              (->Pi "_" a (->Pi "_" list list))
                              (->Pi "nil" list list)))))))

  dhall_clj.ast.ListLength
  (typecheck [this _ctx]
    (->Pi "a"
          (->Const :type)
          (->Pi "_" (->App (->ListT) "a") (->NaturalT))))

  dhall_clj.ast.ListHead
  (typecheck [this _ctx]
    (->Pi "a"
          (->Const :type)
          (->Pi "_"
                (->App (->ListT) "a")
                (->App (->OptionalT) "a"))))

  dhall_clj.ast.ListLast
  (typecheck [this _ctx]
    (->Pi "a"
          (->Const :type)
          (->Pi "_"
                (->App (->ListT) "a")
                (->App (->OptionalT) "a"))))

  dhall_clj.ast.ListIndexed
  (typecheck [this _ctx]
    (let [kts {"index" (->NaturalT)
               "value" (->Var "a" 0)}]
      (->Pi "a"
            (->Const :type)
            (->Pi "_"
                  (->App (->ListT) (->Var "a" 0))
                  (->App (->ListT) (->RecordLit kts))))))

  dhall_clj.ast.ListReverse
  (typecheck [this _ctx]
    (->Pi "a"
          (->Const :type)
          (->Pi "_"
                (->App (->ListT) "a")
                (->App (->ListT) "a"))))

  dhall_clj.ast.OptionalT
  (typecheck [this _ctx]
    (->Pi "_" (->Const :type) (->Const :type)))

  dhall_clj.ast.OptionalLit
  (typecheck [this ctx] "TODO typecheck OptionalLit")

  dhall_clj.ast.OptionalFold
  (typecheck [this _ctx]
    (let [optional (->Var "optional" 0)
          a (->Var "a" 0)
          f (->Pi "optional"
                  (->Const :type)
                  (->Pi "just"
                        (->Pi "_" a optional)
                        (->Pi "nothing" optional optional)))]
      (->Pi "a"
            (->Const :type)
            (->Pi "_"
                  (->App (->OptionalT) a)
                  f))))

  dhall_clj.ast.OptionalBuild
  (typecheck [this ctx]
    (let [optional (->Var "optional" 0)
          a (->Var "a" 0)
          f (->Pi "optional"
                  (->Const :type)
                  (->Pi "just"
                        (->Pi "_" a optional)
                        (->Pi "nothing" optional optional)))]
      (->Pi "a"
            (->Const :type)
            (->Pi "_"
                  f
                  (->App (->OptionalT) a)))))

  dhall_clj.ast.RecordT
  (typecheck [this ctx] "TODO typecheck RecordT")

  dhall_clj.ast.RecordLit
  (typecheck [this ctx] "TODO typecheck RecordLit")

  dhall_clj.ast.UnionT
  (typecheck [this ctx] "TODO typecheck UnionT")

  dhall_clj.ast.UnionLit
  (typecheck [this ctx] "TODO typecheck UnionLit")

  dhall_clj.ast.Combine
  (typecheck [this ctx] "TODO typecheck Combine")

  dhall_clj.ast.CombineTypes
  (typecheck [this ctx] "TODO typecheck CombineTypes")

  dhall_clj.ast.Prefer
  (typecheck [this ctx] "TODO typecheck Prefer")

  dhall_clj.ast.Merge
  (typecheck [this ctx] "TODO typecheck Merge")

  dhall_clj.ast.Constructors
  (typecheck [this ctx] "TODO typecheck Constructors")

  dhall_clj.ast.Field
  (typecheck [this ctx] "TODO typecheck Field")

  dhall_clj.ast.Project
  (typecheck [this ctx] "TODO typecheck Project")

  dhall_clj.ast.ImportAlt
  (typecheck [{:keys [a]} ctx]
    (typecheck a ctx)))
