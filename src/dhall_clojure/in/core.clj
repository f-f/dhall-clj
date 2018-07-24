(ns dhall-clojure.in.core
  (:require [medley.core :refer [map-vals]]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint]]))

(defprotocol Expr
  "Interface that every Expression type should implement"
  (alpha-normalize [this]
    "α-normalize an expression by renaming all variables to `_` and using
    De Bruijn indices to distinguish them")
  (typecheck [this]
    "Typecheck the expression. Returns the type on success, or excepts"))
;;  (emit [this]
;;    "Return a Clojure form from a Dhall expression"))



;; All classes that form the expression tree follow
;;
;;   Implementation note: the classes here are basically the constructors
;;   of the Expr type that the Haskell implementation has in Dhall.Core
;;
;;   However, two constructors are missing here:
;;
;;   - `Note`: this is used there for keeping track of the source position.
;;     Here we use the metadata on nodes for that.
;;
;;   - `Embed`: it's there to make extending the type nice there, while
;;     keeping type safety. Here we just hope we have enough tests :)


(defrecord Const [c])
(defrecord Var [x i])
(defrecord Lam [arg type body])
(defrecord Pi [arg type body])
(defrecord App [a b])
(defrecord Let [label type? body next])
(defrecord Annot [val type])
(defrecord BoolT [])
(defrecord BoolLit [b])
(defrecord BoolAnd [a b])
(defrecord BoolOr [a b])
(defrecord BoolEQ [a b])
(defrecord BoolNE [a b])
(defrecord BoolIf [test then else])
(defrecord NaturalT [])
(defrecord NaturalLit [n])
(defrecord NaturalFold [])
(defrecord NaturalBuild [])
(defrecord NaturalIsZero [])
(defrecord NaturalEven [])
(defrecord NaturalOdd [])
(defrecord NaturalToInteger [])
(defrecord NaturalShow [])
(defrecord NaturalPlus [a b])
(defrecord NaturalTimes [a b])
(defrecord IntegerT [])
(defrecord IntegerLit [n])
(defrecord IntegerShow [])
(defrecord IntegerToDouble [])
(defrecord DoubleT [])
(defrecord DoubleLit [n])
(defrecord DoubleShow [])
(defrecord TextT [])
(defrecord TextLit [chunks])
(defrecord TextAppend [a b])
(defrecord ListT [])
(defrecord ListLit [type? exprs])
(defrecord ListAppend [a b])
(defrecord ListBuild [])
(defrecord ListFold [])
(defrecord ListLength [])
(defrecord ListHead [])
(defrecord ListLast [])
(defrecord ListIndexed [])
(defrecord ListReverse [])
(defrecord OptionalT [])
(defrecord OptionalLit [type val?])
(defrecord OptionalFold [])
(defrecord OptionalBuild [])
(defrecord RecordT [kvs])
(defrecord RecordLit [kvs])
(defrecord UnionT [kvs])
(defrecord UnionLit [k v kvs])
(defrecord Combine [a b])
(defrecord CombineTypes [a b])
(defrecord Prefer [a b])
(defrecord Merge [a b type?])
(defrecord Constructors [e])
(defrecord Field [e k])
(defrecord Project [e ks])
(defrecord ImportAlt [a b])


;; Utils


(defn map-chunks [this f]
  (update this :chunks #(map (fn [el] (if (string? el) el (f el))) %)))



;; Shift

(defprotocol IShift
  (shift [this diff var]
    "`shift` is used by both normalization and type-checking to avoid variable
    capture by shifting variable indices.
    `(shift e diff {:keys [i x]})` modifies the expression `e` by adding `diff`
    to the indices of all variables named `x` whose indices are greater than
    `(+ n m)`, where `m` is the number of bound variables of the same name
    within that scope.
    `diff` is always +1 or -1, because we either:
    * increment variables by `1` to avoid variable capture during substitution
    * decrement variables by `1` when deleting lambdas after substitution"))


(extend-protocol IShift

  Const
  (shift [this diff var] this)

  Var
  (shift [this diff {:keys [x i] :as var}]
    (let [x'  (:x this)
          i'  (:i this)
          i'' (if (and (= x x') (<= i i'))
                (+ i' diff)
                i')]
      (assoc this :i i'')))

  Lam
  (shift [{:keys [arg] :as this} diff {:keys [x i] :as var}]
    (let [i' (if (= x arg)
               (inc i)
               i)]
      (-> this
         (update :type shift diff var)
         (update :body shift diff (assoc var :i i')))))

  Pi
  (shift [{:keys [arg] :as this} diff {:keys [x i] :as var}]
    (let [i' (if (= x arg)
               (inc i)
               i)]
      (-> this
         (update :type shift diff var)
         (update :body shift diff (assoc var :i i')))))

  App
  (shift [this diff var]
    (-> this
       (update :a shift diff var)
       (update :b shift diff var)))

  Let
  (shift [{:keys [label type?] :as this} diff {:keys [x i] :as var}]
    (let [i' (if (= x label)
               (inc i)
               i)]
      (-> this
         (assoc  :type? (when type? (shift type? diff var)))
         (update :body shift diff var)
         (update :next shift diff (assoc var :i i')))))

  Annot
  (shift [this diff var]
    (-> this
       (update :val  shift diff var)
       (update :type shift diff var)))

  BoolT
  (shift [this diff var] this)

  BoolLit
  (shift [this diff var] this)

  BoolAnd
  (shift [this diff var]
    (-> this
       (update :a shift diff var)
       (update :b shift diff var)))

  BoolOr
  (shift [this diff var]
    (-> this
       (update :a shift diff var)
       (update :b shift diff var)))

  BoolEQ
  (shift [this diff var]
    (-> this
       (update :a shift diff var)
       (update :b shift diff var)))

  BoolNE
  (shift [this diff var]
    (-> this
       (update :a shift diff var)
       (update :b shift diff var)))

  BoolIf
  (shift [this diff var]
    (-> this
       (update :test shift diff var)
       (update :then shift diff var)
       (update :else shift diff var)))

  NaturalT
  (shift [this diff var] this)

  NaturalLit
  (shift [this diff var] this)

  NaturalFold
  (shift [this diff var] this)

  NaturalBuild
  (shift [this diff var] this)

  NaturalIsZero
  (shift [this diff var] this)

  NaturalEven
  (shift [this diff var] this)

  NaturalOdd
  (shift [this diff var] this)

  NaturalToInteger
  (shift [this diff var] this)

  NaturalShow
  (shift [this diff var] this)

  NaturalPlus
  (shift [this diff var]
    (-> this
       (update :a shift diff var)
       (update :b shift diff var)))

  NaturalTimes
  (shift [this diff var]
    (-> this
       (update :a shift diff var)
       (update :b shift diff var)))

  IntegerT
  (shift [this diff var] this)

  IntegerLit
  (shift [this diff var] this)

  IntegerShow
  (shift [this diff var] this)

  IntegerToDouble
  (shift [this diff var] this)

  DoubleT
  (shift [this diff var] this)

  DoubleLit
  (shift [this diff var] this)

  DoubleShow
  (shift [this diff var] this)

  TextT
  (shift [this diff var] this)

  TextLit
  (shift [this diff var]
    (map-chunks this (fn [c] (shift c diff var))))

  TextAppend
  (shift [this diff var]
    (-> this
       (update :a shift diff var)
       (update :b shift diff var)))

  ListT
  (shift [this diff var] this)

  ListLit
  (shift [{:keys [type? exprs] :as this} diff var]
    (-> this
       (assoc :type? (when type? (shift type? diff var))
              :exprs (mapv #(shift % diff var) exprs))))

  ListAppend
  (shift [this diff var]
    (-> this
       (update :a shift diff var)
       (update :b shift diff var)))

  ListBuild
  (shift [this diff var] this)

  ListFold
  (shift [this diff var] this)

  ListLength
  (shift [this diff var] this)

  ListHead
  (shift [this diff var] this)

  ListLast
  (shift [this diff var] this)

  ListIndexed
  (shift [this diff var] this)

  ListReverse
  (shift [this diff var] this)

  OptionalT
  (shift [this diff var] this)

  OptionalLit
  (shift [{:keys [val?] :as this} diff var]
    (-> this
       (update :type shift diff var)
       (assoc :val? (when val? (shift val? diff var)))))

  OptionalFold
  (shift [this diff var] this)

  OptionalBuild
  (shift [this diff var] this)

  RecordT
  (shift [this diff var]
    (update this :kvs (fn [kvs] (map-vals #(shift % diff var) kvs))))

  RecordLit
  (shift [this diff var]
    (update this :kvs (fn [kvs] (map-vals #(shift % diff var) kvs))))

  UnionT
  (shift [this diff var]
    (update this :kvs (fn [kvs] (map-vals #(shift % diff var) kvs))))

  UnionLit
  (shift [this diff var]
    (-> this
       (update :v shift diff var)
       (update :kvs (fn [kvs] (map-vals #(shift % diff var) kvs)))))

  Combine
  (shift [this diff var]
    (-> this
       (update :a shift diff var)
       (update :b shift diff var)))

  CombineTypes
  (shift [this diff var]
    (-> this
       (update :a shift diff var)
       (update :b shift diff var)))

  Prefer
  (shift [this diff var]
    (-> this
       (update :a shift diff var)
       (update :b shift diff var)))

  Constructors
  (shift [this diff var]
    (update this :e shift diff var))

  Merge
  (shift [this diff var]
    (let [type?  (:type? this)
          type?' (when type? (shift type? diff var))]
      (-> this
         (update :a shift diff var)
         (update :b shift diff var)
         (assoc :type? type?'))))

  Field
  (shift [this diff var]
    (update this :e shift diff var))

  Project
  (shift [this diff var]
    (update this :e shift diff var))

  ImportAlt
  (shift [this diff var]
    (-> this
       (update :a shift diff var)
       (update :b shift diff var))))



;; Subst

(defprotocol ISubst
  (subst [this var e]
    "Substitute all occurrences of a variable with an expression
    E.g. (subst this var e)  ~  this[var := e]"))


(extend-protocol ISubst

  Const
  (subst [this var e] this)

  Var
  (subst [this var e]
    (if (= this var)
      e
      this))

  Lam
  (subst [this {:keys [x i] :as var} e]
    (let [y (:arg this)
          i' (if (= x y)
               (inc i)
               i)
          e' (shift e 1 (->Var y 0))]
      (-> this
         (update :type subst var               e)
         (update :body subst (assoc var :i i') e'))))

  Pi
  (subst [this {:keys [x i] :as var} e]
    (let [y (:arg this)
          i' (if (= x y)
               (inc i)
               i)
          e' (shift e 1 (->Var y 0))]
      (-> this
         (update :type subst var               e)
         (update :body subst (assoc var :i i') e'))))

  App
  (subst [this var e]
    (-> this
       (update :a subst var e)
       (update :b subst var e)))

  Let
  (subst [{:keys [label type?] :as this} {:keys [x i] :as var} e]
    (let [y  label
          i' (if (= x y)
               (inc i)
               i)]
      (-> this
         (assoc  :type? (when type? (subst type? var e)))
         (update :body subst var e)
         (update :next subst (->Var x i') (shift e 1 (->Var y 0))))))

  Annot
  (subst [this var e]
    (-> this
       (update :val  subst var e)
       (update :type subst var e)))

  BoolT
  (subst [this var e] this)

  BoolLit
  (subst [this var e] this)

  BoolAnd
  (subst [this var e]
    (-> this
       (update :a subst var e)
       (update :b subst var e)))

  BoolOr
  (subst [this var e]
    (-> this
       (update :a subst var e)
       (update :b subst var e)))

  BoolEQ
  (subst [this var e]
    (-> this
       (update :a subst var e)
       (update :b subst var e)))

  BoolNE
  (subst [this var e]
    (-> this
       (update :a subst var e)
       (update :b subst var e)))

  BoolIf
  (subst [this var e]
    (-> this
       (update :test subst var e)
       (update :then subst var e)
       (update :else subst var e)))

  NaturalT
  (subst [this var e] this)

  NaturalLit
  (subst [this var e] this)

  NaturalFold
  (subst [this var e] this)

  NaturalBuild
  (subst [this var e] this)

  NaturalIsZero
  (subst [this var e] this)

  NaturalEven
  (subst [this var e] this)

  NaturalOdd
  (subst [this var e] this)

  NaturalToInteger
  (subst [this var e] this)

  NaturalShow
  (subst [this var e] this)

  NaturalPlus
  (subst [this var e]
    (-> this
       (update :a subst var e)
       (update :b subst var e)))

  NaturalTimes
  (subst [this var e]
    (-> this
       (update :a subst var e)
       (update :b subst var e)))

  IntegerT
  (subst [this var e] this)

  IntegerLit
  (subst [this var e] this)

  IntegerShow
  (subst [this var e] this)

  IntegerToDouble
  (subst [this var e] this)

  DoubleT
  (subst [this var e] this)

  DoubleLit
  (subst [this var e] this)

  DoubleShow
  (subst [this var e] this)

  TextT
  (subst [this var e] this)

  TextLit
  (subst [this var e]
    (map-chunks this (fn [c] (subst c var e))))

  TextAppend
  (subst [this var e]
    (-> this
       (update :a subst var e)
       (update :b subst var e)))

  ListT
  (subst [this var e] this)

  ListLit
  (subst [{:keys [type? exprs] :as this} var e]
    (-> this
       (assoc :type? (when type? (subst type? var e))
              :exprs (mapv #(subst % var e) exprs))))

  ListAppend
  (subst [this var e]
    (-> this
       (update :a subst var e)
       (update :b subst var e)))

  ListBuild
  (subst [this var e] this)

  ListFold
  (subst [this var e] this)

  ListLength
  (subst [this var e] this)

  ListHead
  (subst [this var e] this)

  ListLast
  (subst [this var e] this)

  ListIndexed
  (subst [this var e] this)

  ListReverse
  (subst [this var e] this)

  OptionalT
  (subst [this var e] this)

  OptionalLit
  (subst [{:keys [val?] :as this} var e]
    (-> this
       (update :type subst var e)
       (assoc :val?  (when val? (subst val? var e)))))

  OptionalFold
  (subst [this var e] this)

  OptionalBuild
  (subst [this var e] this)

  RecordT
  (subst [this var e]
    (update this :kvs (fn [kvs] (map-vals #(subst % var e) kvs))))

  RecordLit
  (subst [this var e]
    (update this :kvs (fn [kvs] (map-vals #(subst % var e) kvs))))

  UnionT
  (subst [this var e]
    (update this :kvs (fn [kvs] (map-vals #(subst % var e) kvs))))

  UnionLit
  (subst [this var e]
    (-> this
       (update :v subst var e)
       (update :kvs (fn [kvs] (map-vals #(subst % var e) kvs)))))

  Combine
  (subst [this var e]
    (-> this
       (update :a subst var e)
       (update :b subst var e)))

  CombineTypes
  (subst [this var e]
    (-> this
       (update :a subst var e)
       (update :b subst var e)))

  Prefer
  (subst [this var e]
    (-> this
       (update :a subst var e)
       (update :b subst var e)))

  Merge
  (subst [{:keys [type?] :as this} var e]
    (-> this
       (update :a subst var e)
       (update :b subst var e)
       (assoc :type? (when type? (subst type? var e)))))

  Constructors
  (subst [this var e]
    (update this :e subst var e))

  Field
  (subst [this var e]
    (update this :e subst var e))

  Project
  (subst [this var e]
    (update this :e subst var e))

  ImportAlt
  (subst [this var e]
    (-> this
       (update :a subst var e)
       (update :b subst var e))))


;; beta-normalize

(defprotocol IBetaNormalize
  (beta-normalize [this]
    "Reduce an expression to its normal form, performing beta reduction"))


(defn judgmentally-equal
  "Returns `true` if two expressions are α-equivalent and β-equivalent and
  `false` otherwise"
  [a b]
  (let [ab-normalize (comp beta-normalize alpha-normalize)]
    (= (ab-normalize a)
       (ab-normalize b))))


(extend-protocol IBetaNormalize

  Const
  (beta-normalize [this] this)

  Var
  (beta-normalize [this] this)

  Lam
  (beta-normalize [this]
    (-> this
       (update :type beta-normalize)
       (update :body beta-normalize)))

  Pi
  (beta-normalize [this]
    (-> this
       (update :type beta-normalize)
       (update :body beta-normalize)))

  App
  (beta-normalize [this]
    (let [f  (:a this)
          a  (:b this)
          f' (beta-normalize f)]
      (if (instance? Lam f')
        (let [{:keys [arg type body]} f'
              a' (shift a 1 (->Var arg 0))]
          (-> body
             (subst (->Var arg 0) a')
             (shift -1 (->Var arg 0))
             beta-normalize))
        (let [a' (beta-normalize a)]
          (cond
            ;; build/fold fusion for List
            (and (instance? App f')
                 (instance? ListBuild (:a f'))
                 (instance? App a')
                 (instance? App (:a a'))
                 (instance? ListFold (:a (:a a'))))
            (beta-normalize (:b a'))

            ;; build/fold fusion for Natural
            (and (instance? NaturalBuild f')
                 (instance? App a')
                 (instance? NaturalFold (:a a')))
            (beta-normalize (:b a'))

            ;; build/fold fusion for Optional
            (and (instance? App f')
                 (instance? OptionalBuild (:a f'))
                 (instance? App a')
                 (instance? App (:a a'))
                 (instance? OptionalFold (:a (:a a'))))
            (beta-normalize (:b a'))

            ;; NaturalFold
            (and (instance? App         f')
                 (instance? App         (:a f'))
                 (instance? App         (:a (:a f')))
                 (instance? NaturalFold (:a (:a (:a f'))))
                 (instance? NaturalLit  (:b (:a (:a f')))))
            (let [zero  a'
                  succ' (:b f')
                  t     (:b (:a f'))
                  n0    (:n (:b (:a (:a f'))))
                  _ (declare go)
                  go (fn [n]
                       (if (zero? n)
                         (beta-normalize zero)
                         (beta-normalize (->App succ' (go (dec n))))))]
              (go n0))

            ;; NaturalBuild
            (instance? NaturalBuild f')
            (let [zero (->NaturalLit 0)
                  succ (->Lam "x"
                              (->NaturalT)
                              (->NaturalPlus (->Var "x" 0) (->NaturalLit 1)))]
              (beta-normalize (->App (->App (->App a' (->NaturalT)) succ) zero)))

            ;; NaturalIsZero
            (and (instance? NaturalLit a')
                 (instance? NaturalIsZero f'))
            (->BoolLit (= 0 (:n a')))

            ;; NaturalEven
            (and (instance? NaturalLit a')
                 (instance? NaturalEven f'))
            (->BoolLit (even? (:n a')))

            ;; NaturalOdd
            (and (instance? NaturalLit a')
                 (instance? NaturalOdd f'))
            (->BoolLit (odd? (:n a')))

            ;; NaturalToInteger
            (and (instance? NaturalLit a')
                 (instance? NaturalToInteger f'))
            (->IntegerLit (:n a'))

            ;; NaturalShow
            (and (instance? NaturalLit a')
                 (instance? NaturalShow f'))
            (->TextLit [(str (:n a'))])

            ;; IntegerShow
            (and (instance? IntegerLit a')
                 (instance? IntegerShow f'))
            (->TextLit [(let [n (:n a')]
                          (str (when (>= n 0) "+") n))])

            ;; IntegerToDouble
            (and (instance? IntegerLit a')
                 (instance? IntegerToDouble f'))
            (->DoubleLit (double (:n a')))

            ;; DoubleShow
            (and (instance? DoubleLit a')
                 (instance? DoubleShow f'))
            (->TextLit [(str (:n a'))])

            ;; OptionalBuild
            (and (instance? App f')
                 (instance? OptionalBuild (:a f')))
            (let [typ  (:b f')
                  typ' (shift typ 1 (->Var "a" 0))
                  optional (->App (->OptionalT) typ)
                  just     (->Lam "a"
                                  typ
                                  (->OptionalLit typ'
                                                 (->Var "a" 0)))
                  nothing  (->OptionalLit typ nil)
                  res (->App (->App (->App a' optional)
                                   just)
                             nothing)]
              (beta-normalize res))

            ;; ListBuild
            (and (instance? App f')
                 (instance? ListBuild (:a f')))
            (let [typ  (:b f')
                  typ' (shift typ 1 (->Var "a" 0))
                  _list (->App (->ListT) typ)
                  _cons (->Lam
                          "a"
                          typ
                          (->Lam
                            "as"
                            (->App (->ListT) typ')
                            (->ListAppend
                              (->ListLit nil [(->Var "a" 0)])
                              (->Var "as" 0))))
                  _nil  (->ListLit typ [])]
              (beta-normalize
                (->App (->App (->App a' _list)
                              _cons)
                       _nil)))

            ;; ListFold
            (and (instance? App      f')
                 (instance? App      (:a f'))
                 (instance? App      (:a (:a f')))
                 (instance? App      (:a (:a (:a f'))))
                 (instance? ListFold (:a (:a (:a (:a f')))))
                 (instance? ListLit  (:b (:a (:a f')))))
            (let [_nil  a'
                  _cons (:b f')
                  t     (:b (:a f'))
                  xs    (:exprs (:b (:a (:a f'))))
                  fold  (fn [y ys]
                          (beta-normalize (->App (->App _cons y) ys)))]
              (reduce fold (beta-normalize _nil) xs))

            ;; ListLength
            (and (instance? App f')
                 (instance? ListLength (:a f'))
                 (instance? ListLit a'))
            (->NaturalLit (count (:exprs a')))

            ;; ListHead
            (and (instance? App f')
                 (instance? ListHead (:a f'))
                 (instance? ListLit a'))
            (beta-normalize (->OptionalLit (:b f') (first (:exprs a'))))

            ;; ListLast
            (and (instance? App f')
                 (instance? ListLast (:a f'))
                 (instance? ListLit a'))
            (beta-normalize (->OptionalLit (:b f') (last (:exprs a'))))

            ;; ListIndexed
            (and (instance? App f')
                 (instance? ListIndexed (:a f'))
                 (instance? ListLit a'))
            (let [t2 (->RecordT {"index" (->NaturalT)
                                 "value" (:b f')})
                  xs (:exprs a')
                  typ? (when (empty? xs)
                         t2)
                  adapt (fn [i el]
                          (->RecordLit
                            {"index" (->NaturalLit i)
                             "value" el}))
                  xs' (map-indexed adapt xs)]
              (beta-normalize (->ListLit typ? xs')))

            ;; ListReverse
            (and (instance? App f')
                 (instance? ListReverse (:a f'))
                 (instance? ListLit a'))
            (let [xs   (:exprs a')
                  typ? (when (empty? xs)
                         (:b f'))]
              (beta-normalize (->ListLit typ? (reverse xs))))

            ;; OptionalFold
            (and (instance? App          f')
                 (instance? App          (:a f'))
                 (instance? App          (:a (:a f')))
                 (instance? App          (:a (:a (:a f'))))
                 (instance? OptionalFold (:a (:a (:a (:a f')))))
                 (instance? OptionalLit  (:b (:a (:a f')))))
            (let [nothing  a'
                  just     (:b f')
                  val?     (:val? (:b (:a (:a f'))))]
              (beta-normalize
                (if val?
                  (->App just val?)
                  nothing)))

            :else (->App f' a'))))))

  Let
  (beta-normalize [{:keys [label body next]}]
    (let [var   (->Var label 0)
          body' (shift body 1 var)]
      (beta-normalize
        (-> next
           (subst var body')
           (shift -1 var)))))

  Annot
  (beta-normalize [this]
    (beta-normalize (:val this)))

  BoolT
  (beta-normalize [this] this)

  BoolLit
  (beta-normalize [this] this)

  BoolAnd
  (beta-normalize [{:keys [a b]}]
    (let [decide (fn [l r]
                   (if (instance? BoolLit l)
                     (if (:b l)
                       r
                       l)
                     (if (instance? BoolLit r)
                       (if (:b r)
                         l
                         r)
                       (if (judgmentally-equal l r)
                         l
                         (->BoolAnd l r)))))]
      (decide (beta-normalize a) (beta-normalize b))))

  BoolOr
  (beta-normalize [{:keys [a b]}]
    (let [decide (fn [l r]
                   (if (instance? BoolLit l)
                     (if-not (:b l)
                       r
                       l)
                     (if (instance? BoolLit r)
                       (if-not (:b r)
                         l
                         r)
                       (if (judgmentally-equal l r)
                         l
                         (->BoolAnd l r)))))]
      (decide (beta-normalize a) (beta-normalize b))))

  BoolEQ
  (beta-normalize [{:keys [a b]}]
    (let [decide (fn [l r]
                   (cond
                     (and (instance? BoolLit l) (:b l)) r
                     (and (instance? BoolLit r) (:b r)) l
                     (judgmentally-equal l r)           (->BoolLit true)
                     :else                              (->BoolEQ l r)))]
      (decide (beta-normalize a) (beta-normalize b))))

  BoolNE
  (beta-normalize [{:keys [a b]}]
    (let [decide (fn [l r]
                   (cond
                     (and (instance? BoolLit l) (not (:b l))) r
                     (and (instance? BoolLit r) (not (:b r))) l
                     (judgmentally-equal l r)                 (->BoolLit false)
                     :else                                    (->BoolNE l r)))]
      (decide (beta-normalize a) (beta-normalize b))))

  BoolIf
  (beta-normalize [{:keys [test then else]}]
    (let [decide (fn [test then else]
                   (cond
                     (instance? BoolLit test)       (if (:b test)
                                                      then
                                                      else)
                     (and (instance? BoolLit then)
                          (instance? BoolLit else)
                          (:b then)
                          (not (:b else)))          test
                     (judgmentally-equal then else) then
                     :else                          (->BoolIf test then else)))]
      (decide (beta-normalize test)
              (beta-normalize then)
              (beta-normalize else))))

  NaturalT
  (beta-normalize [this] this)

  NaturalLit
  (beta-normalize [this] this)

  NaturalFold
  (beta-normalize [this] this)

  NaturalBuild
  (beta-normalize [this] this)

  NaturalIsZero
  (beta-normalize [this] this)

  NaturalEven
  (beta-normalize [this] this)

  NaturalOdd
  (beta-normalize [this] this)

  NaturalToInteger
  (beta-normalize [this] this)

  NaturalShow
  (beta-normalize [this] this)

  NaturalPlus
  (beta-normalize [{:keys [a b]}]
    (let [decide (fn [l r]
                   (cond
                     (and (instance? NaturalLit l) (= 0 (:n l))) r
                     (and (instance? NaturalLit r) (= 0 (:n r))) l
                     (and (instance? NaturalLit l)
                          (instance? NaturalLit r))              (->NaturalLit (+ (:n l)
                                                                                  (:n r)))
                     :else                                       (->NaturalPlus l r)))]
      (decide (beta-normalize a) (beta-normalize b))))

  NaturalTimes
  (beta-normalize [{:keys [a b]}]
    (let [decide (fn [l r]
                   (cond
                     (and (instance? NaturalLit l) (= 1 (:n l))) r
                     (and (instance? NaturalLit r) (= 1 (:n r))) l
                     (and (instance? NaturalLit l) (= 0 (:n l))) (->NaturalLit 0)
                     (and (instance? NaturalLit r) (= 0 (:n r))) (->NaturalLit 0)
                     (and (instance? NaturalLit l)
                          (instance? NaturalLit r))              (->NaturalLit (* (:n l)
                                                                                  (:n r)))
                     :else                                       (->NaturalTimes l r)))]
      (decide (beta-normalize a) (beta-normalize b))))

  IntegerT
  (beta-normalize [this] this)

  IntegerLit
  (beta-normalize [this] this)

  IntegerShow
  (beta-normalize [this] this)

  IntegerToDouble
  (beta-normalize [this] this)

  DoubleT
  (beta-normalize [this] this)

  DoubleLit
  (beta-normalize [this] this)

  DoubleShow
  (beta-normalize [this] this)

  TextT
  (beta-normalize [this] this)

  TextLit
  (beta-normalize [this]
    (let [normalized-chunks (:chunks (map-chunks this beta-normalize))
          new-chunks (mapcat #(if (instance? TextLit %)
                                (:chunks %)
                                (list %))
                             normalized-chunks)]
      ;; If we have only an expr we return that
      (if (and (= 1 (count new-chunks))
               (not (string? (first new-chunks))))
        (first new-chunks)
        (->TextLit new-chunks))))

  TextAppend
  (beta-normalize [{:keys [a b]}]
    (let [empty-text? (fn [t]
                        (and (instance? TextLit t)
                             (-> t :chunks first str/blank?)))
          decide (fn [l r]
                   (cond
                     (empty-text? l)             r
                     (empty-text? r)             l
                     (and (instance? TextLit l)
                          (instance? TextLit r)) (->TextLit (concat (:chunks l) (:chunks r)))
                     :else                       (->TextAppend l r)))]
      (decide (beta-normalize a) (beta-normalize b))))

  ListT
  (beta-normalize [this] this)

  ListLit
  (beta-normalize [{:keys [type?] :as this}]
    (-> this
       (assoc :type?  (when type? (beta-normalize type?)))
       (update :exprs (partial map beta-normalize))))

  ListAppend
  (beta-normalize [{:keys [a b]}]
    (let [decide (fn [l r]
                   (cond
                     (and (instance? ListLit l)
                          (empty? l))            r
                     (and (instance? ListLit r)
                          (empty? r))            l
                     (and (instance? ListLit l)
                          (instance? ListLit r)) (update l :exprs concat (:exprs r))
                     :else                       (->ListAppend l r)))]
      (decide (beta-normalize a) (beta-normalize b))))

  ListBuild
  (beta-normalize [this] this)

  ListFold
  (beta-normalize [this] this)

  ListHead
  (beta-normalize [this] this)

  ListLast
  (beta-normalize [this] this)

  ListIndexed
  (beta-normalize [this] this)

  ListReverse
  (beta-normalize [this] this)

  OptionalT
  (beta-normalize [this] this)

  OptionalLit
  (beta-normalize [{:keys [val?] :as this}]
    (-> this
       (update :type beta-normalize)
       (assoc  :val? (when val? (beta-normalize val?)))))

  OptionalFold
  (beta-normalize [this] this)

  OptionalBuild
  (beta-normalize [this] this)

  RecordT
  (beta-normalize [this]
    (update this :kvs (fn [kvs] (map-vals beta-normalize kvs))))

  RecordLit
  (beta-normalize [this]
    (update this :kvs (fn [kvs] (map-vals beta-normalize kvs))))

  UnionT
  (beta-normalize [this]
    (update this :kvs (fn [kvs] (map-vals beta-normalize kvs))))

  UnionLit
  (beta-normalize [this]
    (-> this
       (update :v beta-normalize)
       (update :kvs (fn [kvs] (map-vals beta-normalize kvs)))))

  Combine
  (beta-normalize [{:keys [a b]}]
    (letfn [(decide [l r]
              (cond
                (and (instance? RecordLit l)
                     (empty? l))              r
                (and (instance? RecordLit r)
                     (empty? r))              l
                (and (instance? RecordLit l)
                     (instance? RecordLit r)) (->RecordLit (merge-with decide (:kvs l) (:kvs r)))
                :else                         (->Combine l r)))]
      (decide (beta-normalize a) (beta-normalize b))))

  CombineTypes
  (beta-normalize [{:keys [a b]}]
    (letfn [(decide [l r]
              (cond
                (and (instance? RecordT l)
                     (empty? l))            r
                (and (instance? RecordT r)
                     (empty? r))            l
                (and (instance? RecordT l)
                     (instance? RecordT r)) (->RecordT (merge-with decide (:kvs l) (:kvs r)))
                :else                       (->CombineTypes l r)))]
      (decide (beta-normalize a) (beta-normalize b))))

  Merge
  (beta-normalize [{:keys [a b type?]}]
    (let [a'     (beta-normalize a)
          b'     (beta-normalize b)
          type?' (when type? (beta-normalize type?))]
      (if (and (instance? RecordLit a')
               (instance? UnionLit b')
               (contains? (:kvs a') (:k b')))
        (beta-normalize
          (->App (get (:kvs a') (:k b'))
                 (:v b')))
        (->Merge a' b' type?'))))

  Constructors
  (beta-normalize [this]
    (let [e' (beta-normalize (:e this))]
      (if (instance? UnionT e')
        (let [kts   (:kvs e')
              adapt (fn [[k v]]
                      (->Lam k v (->UnionLit k
                                             (->Var k 0)
                                             (dissoc kts k))))]
          (->RecordLit (map adapt kts)))
        (->Constructors e'))))

  Field
  (beta-normalize [{:keys [e k] :as this}]
    (let [e' (beta-normalize e)]
      (if (instance? RecordLit e')
        (if-let [v (get (:kvs e') k)]
          (beta-normalize v)
          (->Field (->RecordLit (map-vals beta-normalize (:kvs e'))) k))
        (assoc this :e e'))))

  Project
  (beta-normalize [{:keys [e ks] :as this}]
    (let [e' (beta-normalize e)]
      (if (instance? RecordLit e')
        (let [kvs (:kvs e')]
          (if (every? (fn [k] (contains? kvs k)) ks)
            (beta-normalize (->RecordLit (select-keys kvs ks)))
            (->Project (->RecordLit (map-vals beta-normalize kvs)) ks)))
        (assoc this :e e'))))

  ImportAlt
  (beta-normalize [this]
    (beta-normalize (:a this))))


;; Implementation of Expr interface


(extend-protocol Expr

  Const
  (alpha-normalize [this] this)
  (typecheck [this] "TODO typecheck Const")

  Var
  (alpha-normalize [this] this)
  (typecheck [this] "TODO typecheck Var")

  Lam
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
  (typecheck [this] "TODO typecheck Lam")

  Pi
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
  (typecheck [this] "TODO typecheck Lam")

  App
  (alpha-normalize [this]
    (-> this
       (update :a alpha-normalize)
       (update :b alpha-normalize)))
  (typecheck [this] "TODO typecheck App")

  Let
  (alpha-normalize [{:keys [label type? body next] :as this}]
    (let [var   (->Var label 0)
          v'    (->Var "_" 0)
          next' (if (= label "_")
                  (alpha-normalize next)
                  (-> next
                     (shift 1 v')
                     (subst var v')
                     (shift -1 var)
                     (alpha-normalize)))]
      (assoc this
             :label "_"
             :type? (when type? (alpha-normalize type?))
             :body  (alpha-normalize body)
             :next  next')))
  (typecheck [this] "TODO typecheck Let")

  Annot
  (alpha-normalize [this]
    (-> this
       (update :val  alpha-normalize)
       (update :type alpha-normalize)))
  (typecheck [this] "TODO typecheck Annot")

  BoolT
  (alpha-normalize [this] this)
  (typecheck [this]
    (->Const :type))

  BoolLit
  (alpha-normalize [this] this)
  (typecheck [this]      (->BoolT))

  BoolAnd
  (alpha-normalize [this]
    (-> this
       (update :a alpha-normalize)
       (update :b alpha-normalize)))
  (typecheck [this] "TODO typecheck BoolAnd")

  BoolOr
  (alpha-normalize [this]
    (-> this
       (update :a alpha-normalize)
       (update :b alpha-normalize)))
  (typecheck [this] "TODO typecheck BoolOr")

  BoolEQ
  (alpha-normalize [this]
    (-> this
       (update :a alpha-normalize)
       (update :b alpha-normalize)))
  (typecheck [this] "TODO typecheck BoolEQ")

  BoolNE
  (alpha-normalize [this]
    (-> this
       (update :a alpha-normalize)
       (update :b alpha-normalize)))
  (typecheck [this] "TODO typecheck BoolNE")

  BoolIf
  (alpha-normalize [this]
    (-> this
       (update :test alpha-normalize)
       (update :then alpha-normalize)
       (update :else alpha-normalize)))
  (typecheck [this] "TODO typecheck BoolIf")

  NaturalT
  (alpha-normalize [this] this)
  (typecheck [this]
    (->Const :type))

  NaturalLit
  (alpha-normalize [this] this)
  (typecheck [this]
    (->NaturalT))

  NaturalFold
  (alpha-normalize [this] this)
  (typecheck [this] "TODO typecheck NaturalFold")

  NaturalBuild
  (alpha-normalize [this] this)
  (typecheck [this] "TODO typecheck NaturalBuild")

  NaturalIsZero
  (alpha-normalize [this] this)
  (typecheck [this]
    (->Pi "_" (->NaturalT) (->BoolT)))

  NaturalEven
  (alpha-normalize [this] this)
  (typecheck [this]
    (->Pi "_" (->NaturalT) (->BoolT)))

  NaturalOdd
  (alpha-normalize [this] this)
  (typecheck [this]
    (->Pi "_" (->NaturalT) (->BoolT)))

  NaturalToInteger
  (alpha-normalize [this] this)
  (typecheck [this]
    (->Pi "_" (->NaturalT) (->IntegerT)))

  NaturalShow
  (alpha-normalize [this] this)
  (typecheck [this]
    (->Pi "_" (->NaturalT) (->TextT)))

  NaturalPlus
  (alpha-normalize [this]
    (-> this
       (update :a alpha-normalize)
       (update :b alpha-normalize)))
  (typecheck [this] "TODO typecheck NaturalPlus")

  NaturalTimes
  (alpha-normalize [this]
    (-> this
       (update :a alpha-normalize)
       (update :b alpha-normalize)))
  (typecheck [this] "TODO typecheck NaturalTimes")

  IntegerT
  (alpha-normalize [this] this)
  (typecheck [this]
    (->Const :type))

  IntegerLit
  (alpha-normalize [this] this)
  (typecheck [this]
    (->IntegerT))

  IntegerShow
  (alpha-normalize [this] this)
  (typecheck [this]
    (->Pi "_" (->IntegerT) (->TextT)))

  IntegerToDouble
  (alpha-normalize [this] this)
  (typecheck [this]
    (->Pi "_" (->IntegerT) (->DoubleT)))

  DoubleT
  (alpha-normalize [this] this)
  (typecheck [this]
    (->Const :type))

  DoubleLit
  (alpha-normalize [this] this)
  (typecheck [this]
    (->DoubleT))

  DoubleShow
  (alpha-normalize [this] this)
  (typecheck [this]
    (->Pi "_" (->DoubleT) (->TextT)))

  TextT
  (alpha-normalize [this] this)
  (typecheck [this]
    (->Const :type))

  TextLit
  (alpha-normalize [this]
    (map-chunks this alpha-normalize))
  (typecheck [this] "TODO typecheck TextLit")

  TextAppend
  (alpha-normalize [this]
    (-> this
       (update :a alpha-normalize)
       (update :b alpha-normalize)))
  (typecheck [this] "TODO typecheck TextAppend")

  ListT
  (alpha-normalize [this] this)
  (typecheck [this]
    (->Pi "_" (->Const :type) (->Const :type)))

  ListLit
  (alpha-normalize [{:keys [type?] :as this}]
    (-> this
       (assoc :type?  (when type? (alpha-normalize type?)))
       (update :exprs (partial map alpha-normalize))))
  (typecheck [this] "TODO typecheck ListLit")

  ListAppend
  (alpha-normalize [this]
    (-> this
       (update :a alpha-normalize)
       (update :b alpha-normalize)))
  (typecheck [this] "TODO typecheck ListAppend")

  ListBuild
  (alpha-normalize [this] this)
  (typecheck [this] "TODO typecheck ListBuild")

  ListFold
  (alpha-normalize [this] this)
  (typecheck [this] "TODO typecheck ListFold")

  ListLength
  (alpha-normalize [this] this)
  (typecheck [this]
    (->Pi "a"
          (->Const :type)
          (->Pi "_" (->App (->ListT) "a") (->NaturalT))))

  ListHead
  (alpha-normalize [this] this)
  (typecheck [this]
    (->Pi "a"
          (->Const :type)
          (->Pi "_"
                (->App (->ListT) "a")
                (->App (->OptionalT) "a"))))

  ListLast
  (alpha-normalize [this] this)
  (typecheck [this]
    (->Pi "a"
          (->Const :type)
          (->Pi "_"
                (->App (->ListT) "a")
                (->App (->OptionalT) "a"))))

  ListIndexed
  (alpha-normalize [this] this)
  (typecheck [this] "TODO typecheck ListIndexed")

  ListReverse
  (alpha-normalize [this] this)
  (typecheck [this]
    (->Pi "a"
          (->Const :type)
          (->Pi "_"
                (->App (->ListT) "a")
                (->App (->ListT) "a"))))

  OptionalT
  (alpha-normalize [this] this)
  (typecheck [this]
    (->Pi "_" (->Const :type) (->Const :type)))

  OptionalLit
  (alpha-normalize [{:keys [val?] :as this}]
    (-> this
       (update :type alpha-normalize)
       (assoc  :val? (when val? (alpha-normalize val?)))))
  (typecheck [this] "TODO typecheck OptionalLit")

  OptionalFold
  (alpha-normalize [this] this)
  (typecheck [this] "TODO typecheck OptionalFold")

  OptionalBuild
  (alpha-normalize [this] this)
  (typecheck [this] "TODO typecheck OptionalBuild")

  RecordT
  (alpha-normalize [this]
    (update this :kvs (fn [kvs] (map-vals alpha-normalize kvs))))
  (typecheck [this] "TODO typecheck RecordT")

  RecordLit
  (alpha-normalize [this]
    (update this :kvs (fn [kvs] (map-vals alpha-normalize kvs))))
  (typecheck [this] "TODO typecheck RecordLit")

  UnionT
  (alpha-normalize [this]
    (update this :kvs (fn [kvs] (map-vals alpha-normalize kvs))))
  (typecheck [this] "TODO typecheck UnionT")

  UnionLit
  (alpha-normalize [this]
    (-> this
       (update :v alpha-normalize)
       (update :kvs (fn [kvs] (map-vals alpha-normalize kvs)))))
  (typecheck [this] "TODO typecheck UnionLit")

  Combine
  (alpha-normalize [this]
    (-> this
       (update :a alpha-normalize)
       (update :b alpha-normalize)))
  (typecheck [this] "TODO typecheck Combine")

  CombineTypes
  (alpha-normalize [this]
    (-> this
       (update :a alpha-normalize)
       (update :b alpha-normalize)))
  (typecheck [this] "TODO typecheck CombineTypes")

  Prefer
  (alpha-normalize [this]
    (-> this
       (update :a alpha-normalize)
       (update :b alpha-normalize)))
  (beta-normalize [{:keys [a b]}]
    (letfn [(decide [l r]
              (cond
                (and (instance? RecordLit l)
                     (empty? l))              r
                (and (instance? RecordLit r)
                     (empty? r))              l
                (and (instance? RecordLit l)
                     (instance? RecordLit r)) (->RecordLit (merge (:kvs l) (:kvs r)))
                :else                         (->Prefer l r)))]
      (decide (beta-normalize a) (beta-normalize b))))
  (typecheck [this] "TODO typecheck Prefer")

  Merge
  (alpha-normalize [{:keys [type?] :as this}]
    (-> this
       (update :a alpha-normalize)
       (update :b alpha-normalize)
       (assoc :type? (when type? (alpha-normalize type?)))))
  (typecheck [this] "TODO typecheck Merge")

  Constructors
  (alpha-normalize [this]
    (update this :e alpha-normalize))
  (typecheck [this] "TODO typecheck Constructors")

  Field
  (alpha-normalize [this]
    (update this :e alpha-normalize))
  (typecheck [this] "TODO typecheck Field")

  Project
  (alpha-normalize [this]
    (update this :e alpha-normalize))
  (typecheck [this] "TODO typecheck Project")

  ImportAlt
  (alpha-normalize [this]
    (-> this
       (update :a alpha-normalize)
       (update :b alpha-normalize)))
  (typecheck [this] "TODO typecheck ImportAlt"))
