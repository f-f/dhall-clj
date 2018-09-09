(ns dhall-clj.typecheck
  (:require [medley.core :refer [map-kv]]
            [clojure.set :refer [union difference]]
            [dhall-clj.ast :refer :all]
            [dhall-clj.context :as context]
            [dhall-clj.fail :as fail]
            [dhall-clj.beta-normalize :refer [beta-normalize judgmentally-equal]])
  (:import [dhall_clj.ast BoolT NaturalT TextT ListT UnionT App Const Pi RecordT]))


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
         :else (~op-constructor)))))

(defn combine-types
  "Deep merges two maps `ktsA` and `ktsB`, recurring only if the shared
  keys are Records themselves, erroring otherwise."
  [ktsA ktsB error-fn]
  (let [kts (mapv
              (fn [k]
                (let [a (get ktsA k)
                      b (get ktsB k)]
                  (cond
                    (and (instance? RecordT a)
                         (instance? RecordT b))
                    [k (combine-types (:kvs a) (:kvs b) error-fn)]

                    (and a (not b))
                    [k a]

                    (and b (not a))
                    [k b]

                    :else (error-fn {:k k}))))
              (union
                (set (keys ktsA))
                (set (keys ktsB))))]
    (->RecordT (into {} kts))))


(defprotocol ITypecheck
  (typecheck [this context]
    "Typecheck an expression in a context.
    Returns the type on success, or excepts with an
    ex-info of type `:dhall-clj.fail/typecheck`."))


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
  (typecheck [{:keys [arg type body] :as this} ctx]
    (let [typeT (-> type (typecheck ctx) beta-normalize)
          typeK (if (instance? Const typeT)
                  (:c typeT)
                  (fail/invalid-input-type! ctx this {:type type}))
          ctx' (-> (beta-normalize type)
                  (context/insert arg ctx)
                  (context/transform (fn [e] (shift e 1 (->Var arg 0)))))
          bodyT (-> body (typecheck ctx') beta-normalize)
          bodyK (if (instance? Const bodyT)
                  (:c bodyT)
                  (fail/invalid-output-type! ctx' this {:body body}))]
      (if (and (= typeK :type) (= bodyK :kind))
        (fail/no-dependent-types! ctx this {:type type :body body})
        (->Const bodyK))))

  dhall_clj.ast.App
  (typecheck [{:keys [a b] :as this} ctx]
    (let [fT (-> a (typecheck ctx) beta-normalize)
          _ (when-not (instance? Pi fT)
              (fail/not-a-function! ctx this {:f a :f-type fT}))
          {:keys [arg type body]} fT
          type' (typecheck b ctx)]
      (if-not (judgmentally-equal type type')
        (fail/type-mismatch!
          ctx
          this
          {:f a
           :f-input-type (beta-normalize type)
           :a b
           :a-type (beta-normalize type')})
        (let [v (->Var arg 0)
              b' (shift b 1 v)]
          (-> body
             (subst v b')
             (shift -1 v))))))

  dhall_clj.ast.Let
  (typecheck [{:keys [label type? body next] :as this} ctx]
    (let [bodyT (typecheck body ctx)
          ;; FIXME normalize in the else branch also in the haskell implementation
          _ (when type?
              (typecheck type? ctx)
              (when-not (judgmentally-equal type? bodyT)
                (fail/annot-mismatch!
                  ctx
                  this
                  {:val   body
                   :type  (beta-normalize bodyT)
                   :annot (beta-normalize type?)})))
          bodyK (typecheck bodyT ctx)
          v (->Var label 0)
          body' (-> body
                   (beta-normalize)
                   (shift 1 v))]
      ;; If the body's type is Type, we can take a fast path
      ;; and typecheck only at context-insertion-time
      (if (= (->Const :type) (beta-normalize bodyK))
        (let [ctx' (-> (beta-normalize bodyT)
                      (context/insert label ctx)
                      (context/transform (fn [e] (shift e 1 v))))]
          (-> next
             (typecheck ctx')
             (subst v body')
             (shift -1 v)))
        (-> next
           (subst v body')
           (shift -1 v)
           (typecheck ctx)))))


  dhall_clj.ast.Annot
  (typecheck [{:keys [val type] :as this} ctx]
    (let [_     (typecheck type ctx)
          type' (typecheck val  ctx)]
      (if (judgmentally-equal type type')
        type
        (fail/annot-mismatch!
          ctx
          this
          {:val   val
           :type  (beta-normalize type')
           :annot (beta-normalize type)}))))

  dhall_clj.ast.BoolT
  (typecheck [this _ctx]
    (->Const :type))

  dhall_clj.ast.BoolLit
  (typecheck [this _ctx]
    (->BoolT))

  dhall_clj.ast.BoolAnd
  (typecheck [{:keys [a b] :as this} ctx]
    (typecheck-binary this ctx BoolT fail/cant-and!))

  dhall_clj.ast.BoolOr
  (typecheck [{:keys [a b] :as this} ctx]
    (typecheck-binary this ctx BoolT fail/cant-or!))

  dhall_clj.ast.BoolEQ
  (typecheck [{:keys [a b] :as this} ctx]
    (typecheck-binary this ctx BoolT fail/cant-eq!))

  dhall_clj.ast.BoolNE
  (typecheck [{:keys [a b] :as this} ctx]
    (typecheck-binary this ctx BoolT fail/cant-neq!))

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
    (typecheck-binary this ctx NaturalT fail/cant-add!))

  dhall_clj.ast.NaturalTimes
  (typecheck [{:keys [a b] :as this} ctx]
    (typecheck-binary this ctx NaturalT fail/cant-multiply!))

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
  (typecheck [{:keys [chunks] :as this} ctx]
    (mapv
      (fn [c]
        (let [cT (-> c (typecheck ctx) beta-normalize)]
          (when-not (instance? TextT cT)
            (fail/cant-interpolate! ctx this {:chunk c :chunk-type cT}))))
      chunks)
    (->TextT))

  dhall_clj.ast.TextAppend
  (typecheck [{:keys [a b] :as this} ctx]
    (typecheck-binary this ctx TextT fail/cant-text-append!))

  dhall_clj.ast.ListT
  (typecheck [this _ctx]
    (->Pi "_" (->Const :type) (->Const :type)))

  dhall_clj.ast.ListLit
  (typecheck [{:keys [type? exprs] :as this} ctx]
    (let [;; Either we get the type from the annotation or from the first element
          typ (or type?
                  (try
                    (typecheck (first exprs) ctx) ;; FIXME: I'm not super fond of this try
                    (catch Exception e
                      (fail/missing-list-type! ctx this))))
          k (-> typ
               (typecheck ctx)
               (beta-normalize))
          _ (when-not (= (->Const :type) k)
              (fail/invalid-list-type! ctx this {:type typ}))]
      (doall
        (map-indexed
          (fn [i el]
            (let [typ' (typecheck el ctx)]
              (when-not (judgmentally-equal typ typ')
                ((if type?
                   fail/invalid-list-element!
                   fail/mismatched-list-elements!)
                 ctx
                 this
                 {:index i
                  :list-type (beta-normalize typ)
                  :element el
                  :element-type (beta-normalize typ')}))))
          exprs))
      (->App (->ListT) typ)))


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
          (->Pi "_" (->App (->ListT) (->Var "a" 0)) (->NaturalT))))

  dhall_clj.ast.ListHead
  (typecheck [this _ctx]
    (->Pi "a"
          (->Const :type)
          (->Pi "_"
                (->App (->ListT) (->Var "a" 0))
                (->App (->OptionalT) (->Var "a" 0)))))

  dhall_clj.ast.ListLast
  (typecheck [this _ctx]
    (->Pi "a"
          (->Const :type)
          (->Pi "_"
                (->App (->ListT) (->Var "a" 0))
                (->App (->OptionalT) (->Var "a" 0)))))

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
                (->App (->ListT) (->Var "a" 0))
                (->App (->ListT) (->Var "a" 0)))))

  dhall_clj.ast.OptionalT
  (typecheck [this _ctx]
    (->Pi "_" (->Const :type) (->Const :type)))

  dhall_clj.ast.None
  (typecheck [this _ctx]
    (->Pi "A" (->Const :type) (->App (->OptionalT) (->Var "A" 0))))

  dhall_clj.ast.Some
  (typecheck [{:keys [e] :as this} ctx]
    (let [eT (typecheck e ctx)
          k  (-> eT (typecheck ctx) beta-normalize)]
      (when-not (= k (->Const :type))
        (fail/invalid-some! ctx this {:value e :type eT :kind k}))
      (->App (->OptionalT) eT)))

  dhall_clj.ast.OptionalLit
  (typecheck [{:keys [type val?] :as this} ctx]
    (let [k (-> type (typecheck ctx) beta-normalize)]
      (when-not (= k (->Const :type))
        (fail/invalid-optional-type! ctx this {:type type}))
      (when val?
        (let [type' (typecheck val? ctx)]
          (when-not (judgmentally-equal type type')
            (fail/invalid-optional-element!
              {:type (beta-normalize type)
               :value val?
               :value-type (beta-normalize val?)}))))
      (->App (->OptionalT) type)))

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
  (typecheck [{:keys [kvs] :as this} ctx]
    (->Const
      (if (empty? kvs)
        :type
        (let [[k0 t0] (first kvs)
              kind0 (-> t0 (typecheck ctx) beta-normalize)
              c (cond
                  (= kind0 (->Const :type))
                  :type

                  (and (= kind0 (->Const :kind))
                       (judgmentally-equal t0 (->Const :type)))
                  :kind

                  :else (fail/invalid-field-type! ctx this {:key k0 :type t0}))]
          (mapv
            (fn [[k t]]
              (condp = (-> t (typecheck ctx) beta-normalize)
                (->Const :type) (when-not (= c :type)
                                  (fail/field-annotation-mismatch!
                                    ctx
                                    this
                                    {:key k :type t :key0 k0 :type0 t0 :meta :type}))
                (->Const :kind) (if-not (= c :kind)
                                  (fail/field-annotation-mismatch!
                                    ctx
                                    this
                                    {:key k :type t :key0 k0 :type0 t0 :meta :kind})
                                  (when-not (judgmentally-equal t (->Const :type))
                                    (fail/invalid-field-type! ctx this {:key k :type t})))
                (fail/invalid-field-type! ctx this {:key k :type t})))
            (rest kvs))
          c))))

  dhall_clj.ast.RecordLit
  (typecheck [{:keys [kvs] :as this} ctx]
    (->RecordT
      (if (empty? kvs)
        {}
        (let [[k0 v0] (first kvs)
              t0 (typecheck v0 ctx)
              kind0 (-> t0 (typecheck ctx) beta-normalize)
              c (cond
                  (= kind0 (->Const :type))
                  :type

                  (and (= kind0 (->Const :kind))
                       (judgmentally-equal t0 (->Const :type)))
                  :kind

                  :else (fail/invalid-field-type! ctx this {:key k0 :value v0}))]
          (map-kv
            (fn [k v]
              (let [t (typecheck v ctx)]
                (condp = (-> t (typecheck ctx) beta-normalize)
                  (->Const :type) (when-not (= c :type)
                                    (fail/field-mismatch!
                                      ctx
                                      this
                                      {:key k :value v :key0 k0 :value0 v0 :meta :type}))
                  (->Const :kind) (if-not (= c :kind)
                                    (fail/field-mismatch!
                                      ctx
                                      this
                                      {:key k :value v :key0 k0 :value0 v0 :meta :kind})
                                    (when-not (judgmentally-equal t (->Const :type))
                                      (fail/invalid-field-type! ctx this {:key k :type t})))
                  (fail/invalid-field! ctx this {:key k :type t}))
                [k t]))
            kvs)))))

  dhall_clj.ast.UnionT
  (typecheck [{:keys [kvs] :as this} ctx]
    (mapv
      (fn [[k t]]
        (when-not (instance? Const (-> t (typecheck ctx) beta-normalize))
          (fail/invalid-alternative-type! ctx this {:k k :t t})))
      kvs)
    (->Const :type))

  dhall_clj.ast.UnionLit
  (typecheck [{:keys [k v kvs] :as this} ctx]
    (when (contains? kvs k)
      (fail/duplicate-alternative! ctx this {:k k}))
    (let [typ (typecheck v ctx)
          union (->UnionT (assoc kvs k (beta-normalize typ)))
          _ (typecheck union ctx)]
      union))

  dhall_clj.ast.Combine
  (typecheck [{:keys [a b] :as this} ctx]
    (let [aT (-> a (typecheck ctx) beta-normalize)
          ktsA (if (instance? RecordT aT)
                 (:kvs aT)
                 (fail/must-combine-a-record! ctx this {:op "∧" :a a :a-type aT}))
          bT (-> b (typecheck ctx) beta-normalize)
          ktsB (if (instance? RecordT bT)
                 (:kvs bT)
                 (fail/must-combine-a-record! ctx this {:op "∧" :b b :b-type bT}))
          aK (-> aT (typecheck ctx) beta-normalize)
          constA (if (instance? Const aK)
                   (:c aK)
                   (fail/must-combine-a-record! ctx this {:op "∧" :a a :a-type aT}))
          bK (-> bT (typecheck ctx) beta-normalize)
          constB (if (instance? Const bK)
                   (:c bK)
                   (fail/must-combine-a-record! ctx this {:op "∧" :b b :b-type bT}))]
      (when (not= constA constB)
        (fail/record-mismatch! ctx this {:op "∧" :a a :b b :a-order constA :b-order constB}))
      (combine-types ktsA ktsB (partial fail/field-collision! ctx this))))

  dhall_clj.ast.CombineTypes
  (typecheck [{:keys [a b] :as this} ctx]
    (let [aT (typecheck a ctx)
          a' (beta-normalize a)
          constA (if (instance? Const aT)
                   (:c aT)
                   (fail/combine-records-requires-record-type! ctx this {:a a :a-normalized a'}))
          bT (typecheck b ctx)
          b' (beta-normalize b)
          constB (if (instance? Const bT)
                   (:c bT)
                   (fail/combine-records-requires-record-type! ctx this {:b b :b-normalized b'}))
          const (if (= constA constB)
                  constA
                  (fail/record-type-mismatch! ctx this {:a a :a-const constA :b b :b-const constB}))
          ktsA (if (instance? RecordT a')
                 (:kvs a')
                 (fail/combine-records-requires-record-type! ctx this {:a a :a-normalized a'}))
          ktsB (if (instance? RecordT b')
                 (:kvs b')
                 (fail/combine-records-requires-record-type! ctx this {:b b :b-normalized b'}))]
      (combine-types ktsA ktsB (partial fail/field-collision! ctx this))
      (->Const const)))

  dhall_clj.ast.Prefer
  (typecheck [{:keys [a b] :as this} ctx]
    (let [aT (-> a (typecheck ctx) beta-normalize)
          ktsA (if (instance? RecordT aT)
                 (:kvs aT)
                 (fail/must-combine-a-record! ctx this {:op "//" :a a :a-type aT}))
          bT (-> b (typecheck ctx) beta-normalize)
          ktsB (if (instance? RecordT bT)
                 (:kvs bT)
                 (fail/must-combine-a-record! ctx this {:op "//" :b b :b-type bT}))
          aK (-> aT (typecheck ctx) beta-normalize)
          constA (if (instance? Const aK)
                   (:c aK)
                   (fail/must-combine-a-record! ctx this {:op "//" :a a :a-type aT}))
          bK (-> bT (typecheck ctx) beta-normalize)
          constB (if (instance? Const bK)
                   (:c bK)
                   (fail/must-combine-a-record! ctx this {:op "//" :b b :b-type bT}))]
      (when (not= constA constB)
        (fail/record-mismatch! ctx this {:op "//" :a a :b b :a-order constA :b-order constB}))
      (->RecordT (merge ktsA ktsB))))

  dhall_clj.ast.Merge
  (typecheck [{:keys [a b type?] :as this} ctx]
    (let [_ (when type?
              (typecheck type? ctx))
          aT (-> a (typecheck ctx) beta-normalize)
          ktsA (if (instance? RecordT aT)
                 (:kvs aT)
                 (fail/must-merge-a-record! ctx this {:a a :a-type aT}))
          bT (-> b (typecheck ctx) beta-normalize)
          ktsB (if (instance? UnionT bT)
                 (:kvs bT)
                 (fail/must-merge-union! ctx this {:b b :b-type bT}))
          keysA (set (keys ktsA))
          keysB (set (keys ktsB))
          diffA (difference keysA keysB)
          diffB (difference keysB keysA)
          _ (when-not (empty? diffA)
              (fail/unused-handler! ctx this {:unused diffA}))
          t (if type?
              type?
              (if (empty? ktsA)
                (fail/missing-merge-type! ctx this)
                (let [[k t] (first ktsA)]
                  (if-not (instance? Pi t)
                    (fail/handler-not-a-function! ctx this {:key k :handler t})
                    (shift (:body t) -1 (->Var (:arg t) 0))))))]
      (mapv
        (fn [[kB tB]]
          (if-let [tA (get ktsA kB)]
            (if (instance? Pi tA)
              (let [{:keys [arg type body]} tA
                    _ (when-not (judgmentally-equal tB type)
                        (fail/handler-input-type-mismatch!
                          ctx
                          this
                          {:key kB :type tB :annotation type}))
                    body' (shift body -1 (->Var arg 0))]
                (when-not (judgmentally-equal t body')
                  (if type?
                    (fail/invalid-handler-output-type!
                      ctx
                      this
                      {:key kB :fn-ann body' :merge-ann t})
                    (fail/handler-output-type-mismatch!
                      ctx
                      this
                      {:key-record (ffirst ktsA) :type t :key-union kB :body-type body'}))))
              (fail/handler-not-a-function! ctx this {:handler tA :key kB}))
            (fail/missing-handler! ctx this {:excess diffB})))

        ktsB)
      t))

  dhall_clj.ast.Constructors
  (typecheck [{:keys [e] :as this} ctx]
    (typecheck e ctx)
    (let [e'  (beta-normalize e)
          kts (if (instance? UnionT e')
                (:kvs e')
                (fail/constructors-require-a-union-type!
                  ctx
                  this
                  {:type e :type-normal e'}))]
      (->RecordT (map-kv
                   (fn [k v] [k (->Pi k v (->UnionT kts))])
                   kts))))

  dhall_clj.ast.Field
  (typecheck [{:keys [e k] :as this} ctx]
    (let [eT (-> e (typecheck ctx) beta-normalize)]
      (if-not (instance? RecordT eT)
        (fail/not-a-record! ctx this {:key k :record e :record-type eT})
        (let [kvs (:kvs eT)]
          (typecheck eT ctx) ;; FIXME: is this actually needed?
          (if (contains? kvs k)
            (get kvs k)
            (fail/missing-field! ctx this {:key k :type eT}))))))

  dhall_clj.ast.Project
  (typecheck [{:keys [e ks] :as this} ctx]
    (let [eT (-> e (typecheck ctx) beta-normalize)]
      (if-not (instance? RecordT eT)
        (fail/not-a-record! ctx this {:keys ks :record e :record-type eT})
        (let [kvs (:kvs eT)
              _   (typecheck eT ctx) ;; FIXME: is this actually needed?
              extract (fn [k]
                        (if (contains? kvs k)
                          [k (get kvs k)]
                          (fail/missing-field! ctx this {:key k :type eT})))]
          (->RecordT (->> ks
                        (mapv extract)
                        (into {})))))))

  dhall_clj.ast.ImportAlt
  (typecheck [{:keys [a]} ctx]
    (typecheck a ctx)))
