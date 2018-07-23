(ns dhall-clojure.in.core
  (:require [medley.core :refer [map-vals]]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint]]))

(defprotocol Expr
  "Interface that every Expression type should implement"
  (subst [this var e]
    "Substitute all occurrences of a variable with an expression
    E.g. (subst this var e)  ~  this[var := e]")
  (alphaNormalize [this]
    "α-normalize an expression by renaming all variables to `_` and using
    De Bruijn indices to distinguish them")
  (normalize [this]
    "Reduce an expression to its normal form, performing beta reduction")
  (typecheck [this]
    "Typecheck the expression. Returns the type on success, or excepts"))
;;  (emit [this]
;;    "Return a Clojure form from a Dhall expression"))


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
  (shift [{:keys [arg type body]} diff {:keys [x i] :as var}]
    (let [i' (if (= x arg)
               (inc i)
               i)
          type' (shift type diff var)
          body' (shift body diff (->Var x i'))]
      (->Lam arg type' body')))

  Pi
  (shift [{:keys [arg type body]} diff {:keys [x i] :as var}]
    (let [i' (if (= x arg)
               (inc i)
               i)
          type' (shift type diff var)
          body' (shift body diff (->Var x i'))]
      (->Pi arg type' body')))

  App
  (shift [this diff var]
    (-> this
       (update :a shift diff var)
       (update :b shift diff var)))

  Let
  (shift [{:keys [label type? body next]} diff {:keys [x i] :as var}]
    (let [i' (if (= x label)
               (inc i)
               i)
          type?' (when type? (shift type? diff var))
          body'  (shift body diff var)
          next'  (shift next diff (->Var x i'))]
      (->Let label type?' body' next')))

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
    (map-chunks this (fn [c] (subst c diff var))))

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



;; Implementation of Expr interface


(defn judgmentallyEqual
  "Returns `true` if two expressions are α-equivalent and β-equivalent and
  `false` otherwise"
  [a b]
  (let [alphaBetaNormalize (comp normalize alphaNormalize)]
    (= (alphaBetaNormalize a)
       (alphaBetaNormalize b))))



(extend-protocol Expr

  Const
  (subst [this var e]    this)
  (alphaNormalize [this] this)
  (normalize [this]      this)
  (typecheck [this] "TODO typecheck Const")



  Var
  (subst [this var e]
    (if (= this var)
      e
      this))
  (alphaNormalize [this] this)
  (normalize [this] this)
  (typecheck [this] "TODO typecheck Var")



  Lam
  (subst [this {:keys [x i] :as var} e]
    (let [y (:arg this)
          i' (if (= x y)
               (inc i)
               i)]
      (-> this
         (update :type subst var e)
         (update :body subst (->Var y i') (shift e 1 (->Var y 0))))))
  (alphaNormalize [{:keys [arg type body] :as this}]
    (let [v1 (shift (->Var "_" 0) 1 (->Var arg 0))]
      (assoc this
             :arg "_"
             :type (alphaNormalize type)
             :body (-> body
                      (subst (->Var arg 0) v1)
                      (shift -1 (->Var arg 0))
                      (alphaNormalize)))))
  (normalize [this]
    (-> this
       (update :type normalize)
       (update :body normalize)))
  (typecheck [this] "TODO typecheck Lam")



  Pi
  (subst [this {:keys [x i] :as var} e]
    (let [y (:arg this)
          i' (if (= x y)
               (inc i)
               i)]
      (-> this
         (update :type subst var e)
         (update :body subst (->Var y i') (shift e 1 (->Var y 0))))))
  (alphaNormalize [{:keys [arg type body] :as this}]
    (let [v1 (shift (->Var "_" 0) 1 (->Var arg 0))]
      (assoc this
             :arg "_"
             :type (alphaNormalize type)
             :body (-> body
                      (subst (->Var arg 0) v1)
                      (shift -1 (->Var arg 0))
                      (alphaNormalize)))))
  (normalize [this]
    (-> this
       (update :type normalize)
       (update :body normalize)))
  (typecheck [this] "TODO typecheck Lam")



  App
  (subst [this var e]
    (-> this
       (update :a subst var e)
       (update :b subst var e)))
  (alphaNormalize [this]
    (-> this
       (update :a alphaNormalize)
       (update :b alphaNormalize)))
  (normalize [this]
    (let [f  (:a this)
          a  (:b this)
          f' (normalize f)]
      (if (instance? Lam f')
        (let [{:keys [arg type body]} f'
              a' (shift a 1 (->Var arg 0))]
          (-> body
             (subst (->Var arg 0) a')
             (shift -1 (->Var arg 0))
             normalize))
        (let [a' (normalize a)]
          (cond
            ;; build/fold fusion for List
            (and (instance? App f')
                 (instance? ListBuild (:a f'))
                 (instance? App a')
                 (instance? App (:a a'))
                 (instance? ListFold (:a (:a a'))))
            (normalize (:b a'))

            ;; build/fold fusion for Natural
            (and (instance? NaturalBuild f')
                 (instance? App a')
                 (instance? NaturalFold (:a a')))
            (normalize (:b a'))

            ;; build/fold fusion for Optional
            (and (instance? App f')
                 (instance? OptionalBuild (:a f'))
                 (instance? App a')
                 (instance? App (:a a'))
                 (instance? OptionalFold (:a (:a a'))))
            (normalize (:b a'))

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
                         (normalize zero)
                         (normalize (->App succ' (go (dec n))))))]
              (go n0))

            ;; NaturalBuild
            (instance? NaturalBuild f')
            (let [zero (->NaturalLit 0)
                  succ (->Lam "x"
                              (->NaturalT)
                              (->NaturalPlus (->Var "x" 0) (->NaturalLit 1)))]
              (normalize (->App (->App (->App a' (->NaturalT)) succ) zero)))

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
              (normalize res))

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
              (normalize (->App (->App (->App a' _list)
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
                          (normalize (->App (->App _cons y) ys)))]
              (reduce fold (normalize _nil) xs))

            ;; ListLength
            (and (instance? App f')
                 (instance? ListLength (:a f'))
                 (instance? ListLit a'))
            (->NaturalLit (count (:exprs a')))

            ;; ListHead
            (and (instance? App f')
                 (instance? ListHead (:a f'))
                 (instance? ListLit a'))
            (normalize (->OptionalLit (:b f') (first (:exprs a'))))

            ;; ListLast
            (and (instance? App f')
                 (instance? ListLast (:a f'))
                 (instance? ListLit a'))
            (normalize (->OptionalLit (:b f') (last (:exprs a'))))

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
              (normalize (->ListLit typ? xs')))

            ;; ListReverse
            (and (instance? App f')
                 (instance? ListReverse (:a f'))
                 (instance? ListLit a'))
            (let [xs   (:exprs a')
                  typ? (when (empty? xs)
                         (:b f'))]
              (normalize (->ListLit typ? (reverse xs))))

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
              (normalize (if val?
                           (->App just val?)
                           nothing)))

            :else (->App f' a'))))))
  (typecheck [this] "TODO typecheck App")



  Let
  (subst [this {:keys [x i] :as var} e]
    (let [y  (:label this)
          i' (if (= x y)
               (inc i)
               i)
          type?  (:type? this)
          type?' (when type? (subst type? var e))]
      (-> this
         (assoc  :type? type?')
         (update :body subst var e)
         (update :next subst (->Var x i') (shift e 1 (->Var y 0))))))
  (alphaNormalize [{:keys [label type? body next] :as this}]
    (let [var (->Var label 0)
          v' (shift (->Var "_" 0) 1 var)]
      (assoc this
             :label "_"
             :type? (when type? (alphaNormalize type?))
             :body  (alphaNormalize body)
             :next  (-> next
                       (subst var v')
                       (shift -1 var)
                       alphaNormalize))))
  (normalize [{:keys [label body next]}]
    (let [var   (->Var label 0)
          body' (shift body 1 var)]
      (normalize (-> next
                    (subst var body')
                    (shift -1 var)))))
  (typecheck [this] "TODO typecheck Let")



  Annot
  (subst [this var e]
    (-> this
       (update :val  subst var e)
       (update :type subst var e)))
  (alphaNormalize [this]
    (-> this
       (update :val  alphaNormalize)
       (update :type alphaNormalize)))
  (normalize [this]
    (normalize (:val this)))
  (typecheck [this] "TODO typecheck Annot")


  BoolT
  (subst [this var e]    this)
  (alphaNormalize [this] this)
  (normalize [this]      this)
  (typecheck [this]
    (->Const :type))



  BoolLit
  (subst [this var e]    this)
  (alphaNormalize [this] this)
  (normalize [this]      this)
  (typecheck [this]      (->BoolT))



  BoolAnd
  (subst [this var e]
    (-> this
       (update :a subst var e)
       (update :b subst var e)))
  (alphaNormalize [this]
    (-> this
       (update :a alphaNormalize)
       (update :b alphaNormalize)))
  (normalize [{:keys [a b]}]
    (let [decide (fn [l r]
                   (if (instance? BoolLit l)
                     (if (:b l)
                       r
                       l)
                     (if (instance? BoolLit r)
                       (if (:b r)
                         l
                         r)
                       (if (judgmentallyEqual l r)
                         l
                         (->BoolAnd l r)))))]
      (decide (normalize a) (normalize b))))
  (typecheck [this] "TODO typecheck BoolAnd")



  BoolOr
  (subst [this var e]
    (-> this
       (update :a subst var e)
       (update :b subst var e)))
  (alphaNormalize [this]
    (-> this
       (update :a alphaNormalize)
       (update :b alphaNormalize)))
  (normalize [{:keys [a b]}]
    (let [decide (fn [l r]
                   (if (instance? BoolLit l)
                     (if-not (:b l)
                       r
                       l)
                     (if (instance? BoolLit r)
                       (if-not (:b r)
                         l
                         r)
                       (if (judgmentallyEqual l r)
                         l
                         (->BoolAnd l r)))))]
      (decide (normalize a) (normalize b))))
  (typecheck [this] "TODO typecheck BoolOr")



  BoolEQ
  (subst [this var e]
    (-> this
       (update :a subst var e)
       (update :b subst var e)))
  (alphaNormalize [this]
    (-> this
       (update :a alphaNormalize)
       (update :b alphaNormalize)))
  (normalize [{:keys [a b]}]
    (let [decide (fn [l r]
                   (cond
                     (and (instance? BoolLit l) (:b l)) r
                     (and (instance? BoolLit r) (:b r)) l
                     (judgmentallyEqual l r)            (->BoolLit true)
                     :else                              (->BoolEQ l r)))]
      (decide (normalize a) (normalize b))))
  (typecheck [this] "TODO typecheck BoolEQ")



  BoolNE
  (subst [this var e]
    (-> this
       (update :a subst var e)
       (update :b subst var e)))
  (alphaNormalize [this]
    (-> this
       (update :a alphaNormalize)
       (update :b alphaNormalize)))
  (normalize [{:keys [a b]}]
    (let [decide (fn [l r]
                   (cond
                     (and (instance? BoolLit l) (not (:b l))) r
                     (and (instance? BoolLit r) (not (:b r))) l
                     (judgmentallyEqual l r)                  (->BoolLit false)
                     :else                                    (->BoolNE l r)))]
      (decide (normalize a) (normalize b))))
  (typecheck [this] "TODO typecheck BoolNE")



  BoolIf
  (subst [this var e]
    (-> this
       (update :test subst var e)
       (update :then subst var e)
       (update :else subst var e)))
  (alphaNormalize [this]
    (-> this
       (update :test alphaNormalize)
       (update :then alphaNormalize)
       (update :else alphaNormalize)))
  (normalize [{:keys [test then else]}]
    (let [decide (fn [test then else]
                   (cond
                     (instance? BoolLit test)      (if (:b test)
                                                     then
                                                     else)
                     (and (instance? BoolLit then)
                          (instance? BoolLit else)
                          (:b then)
                          (not (:b else)))         test
                     (judgmentallyEqual then else) then
                     :else                         (->BoolIf test then else)))]
      (decide (normalize test) (normalize then) (normalize else))))
  (typecheck [this] "TODO typecheck BoolIf")



  NaturalT
  (subst [this var e]    this)
  (alphaNormalize [this] this)
  (normalize [this]      this)
  (typecheck [this]
    (->Const :type))



  NaturalLit
  (subst [this var e]    this)
  (alphaNormalize [this] this)
  (normalize [this]      this)
  (typecheck [this]
    (->NaturalT))



  NaturalFold
  (subst [this var e]    this)
  (alphaNormalize [this] this)
  (normalize [this]      this)
  (typecheck [this] "TODO typecheck NaturalFold")



  NaturalBuild
  (subst [this var e]    this)
  (alphaNormalize [this] this)
  (normalize [this]      this)
  (typecheck [this] "TODO typecheck NaturalBuild")



  NaturalIsZero
  (subst [this var e]    this)
  (alphaNormalize [this] this)
  (normalize [this]      this)
  (typecheck [this]
    (->Pi "_" (->NaturalT) (->BoolT)))



  NaturalEven
  (subst [this var e]    this)
  (alphaNormalize [this] this)
  (normalize [this]      this)
  (typecheck [this]
    (->Pi "_" (->NaturalT) (->BoolT)))



  NaturalOdd
  (subst [this var e]    this)
  (alphaNormalize [this] this)
  (normalize [this]      this)
  (typecheck [this]
    (->Pi "_" (->NaturalT) (->BoolT)))



  NaturalToInteger
  (subst [this var e]    this)
  (alphaNormalize [this] this)
  (normalize [this]      this)
  (typecheck [this]
    (->Pi "_" (->NaturalT) (->IntegerT)))



  NaturalShow
  (subst [this var e]    this)
  (alphaNormalize [this] this)
  (normalize [this]      this)
  (typecheck [this]
    (->Pi "_" (->NaturalT) (->TextT)))



  NaturalPlus
  (subst [this var e]
    (-> this
       (update :a subst var e)
       (update :b subst var e)))
  (alphaNormalize [this]
    (-> this
       (update :a alphaNormalize)
       (update :b alphaNormalize)))
  (normalize [{:keys [a b]}]
    (let [decide (fn [l r]
                   (cond
                     (and (instance? NaturalLit l) (= 0 (:n l))) r
                     (and (instance? NaturalLit r) (= 0 (:n r))) l
                     (and (instance? NaturalLit l)
                          (instance? NaturalLit r))              (->NaturalLit (+ (:n l)
                                                                                  (:n r)))
                     :else                                       (->NaturalPlus l r)))]
      (decide (normalize a) (normalize b))))
  (typecheck [this] "TODO typecheck NaturalPlus")



  NaturalTimes
  (subst [this var e]
    (-> this
       (update :a subst var e)
       (update :b subst var e)))
  (alphaNormalize [this]
    (-> this
       (update :a alphaNormalize)
       (update :b alphaNormalize)))
  (normalize [{:keys [a b]}]
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
      (decide (normalize a) (normalize b))))
  (typecheck [this] "TODO typecheck NaturalTimes")



  IntegerT
  (subst [this var e]    this)
  (alphaNormalize [this] this)
  (normalize [this]      this)
  (typecheck [this]
    (->Const :type))



  IntegerLit
  (subst [this var e]    this)
  (alphaNormalize [this] this)
  (normalize [this]      this)
  (typecheck [this]
    (->IntegerT))



  IntegerShow
  (subst [this var e]    this)
  (alphaNormalize [this] this)
  (normalize [this]      this)
  (typecheck [this]
    (->Pi "_" (->IntegerT) (->TextT)))



  IntegerToDouble
  (subst [this var e]    this)
  (alphaNormalize [this] this)
  (normalize [this]      this)
  (typecheck [this]
    (->Pi "_" (->IntegerT) (->DoubleT)))


  DoubleT
  (subst [this var e]    this)
  (alphaNormalize [this] this)
  (normalize [this]      this)
  (typecheck [this]
    (->Const :type))



  DoubleLit
  (subst [this var e]    this)
  (alphaNormalize [this] this)
  (normalize [this]      this)
  (typecheck [this]
    (->DoubleT))



  DoubleShow
  (subst [this var e]    this)
  (alphaNormalize [this] this)
  (normalize [this]      this)
  (typecheck [this]
    (->Pi "_" (->DoubleT) (->TextT)))



  TextT
  (subst [this var e]    this)
  (alphaNormalize [this] this)
  (normalize [this]      this)
  (typecheck [this]
    (->Const :type))



  TextLit
  (subst [this var e]
    (map-chunks this (fn [c] (subst c var e))))
  (alphaNormalize [this]
    (map-chunks this alphaNormalize))
  (normalize [this]
    (let [normalized-chunks (:chunks (map-chunks this normalize))
          new-chunks (mapcat #(if (instance? TextLit %)
                                (:chunks %)
                                (list %))
                             normalized-chunks)]
      ;; If we have only an expr we return that
      (if (and (= 1 (count new-chunks))
               (not (string? (first new-chunks))))
        (first new-chunks)
        (->TextLit new-chunks))))
  (typecheck [this] "TODO typecheck TextLit")



  TextAppend
  (subst [this var e]
    (-> this
       (update :a subst var e)
       (update :b subst var e)))
  (alphaNormalize [this]
    (-> this
       (update :a alphaNormalize)
       (update :b alphaNormalize)))
  (normalize [{:keys [a b]}]
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
      (decide (normalize a) (normalize b))))
  (typecheck [this] "TODO typecheck TextAppend")



  ListT
  (subst [this var e]    this)
  (alphaNormalize [this] this)
  (normalize [this]      this)
  (typecheck [this]
    (->Pi "_" (->Const :type) (->Const :type)))



  ListLit
  (subst [this var e]
    (let [type?  (:type? this)
          exprs  (:exprs this)
          type?' (when type? (subst type? var e))
          exprs' (mapv #(subst % var e) exprs)]
      (-> this
         (assoc :type? type?'
                :exprs exprs'))))
  (alphaNormalize [{:keys [type?] :as this}]
    (-> this
       (assoc :type?  (when type? (alphaNormalize type?)))
       (update :exprs (partial map alphaNormalize))))
  (normalize [{:keys [type?] :as this}]
    (-> this
       (assoc :type?  (when type? (normalize type?)))
       (update :exprs (partial map normalize))))
  (typecheck [this] "TODO typecheck ListLit")


  ListAppend
  (subst [this var e]
    (-> this
       (update :a subst var e)
       (update :b subst var e)))
  (alphaNormalize [this]
    (-> this
       (update :a alphaNormalize)
       (update :b alphaNormalize)))
  (normalize [{:keys [a b]}]
    (let [decide (fn [l r]
                   (cond
                     (and (instance? ListLit l)
                          (empty? l))            r
                     (and (instance? ListLit r)
                          (empty?))              l
                     (and (instance? TextLit l)
                          (instance? TextLit r)) (update l :exprs concat (:exprs r))
                     :else                       (->ListAppend l r)))]
      (decide (normalize a) (normalize b))))
  (typecheck [this] "TODO typecheck ListAppend")



  ListBuild
  (subst [this var e]    this)
  (alphaNormalize [this] this)
  (normalize [this]      this)
  (typecheck [this] "TODO typecheck ListBuild")



  ListFold
  (subst [this var e]    this)
  (alphaNormalize [this] this)
  (normalize [this]      this)
  (typecheck [this] "TODO typecheck ListFold")



  ListLength
  (subst [this var e]    this)
  (alphaNormalize [this] this)
  (normalize [this]      this)
  (typecheck [this]
    (->Pi "a"
          (->Const :type)
          (->Pi "_" (->App (->ListT) "a") (->NaturalT))))



  ListHead
  (subst [this var e]    this)
  (alphaNormalize [this] this)
  (normalize [this]      this)
  (typecheck [this]
    (->Pi "a"
          (->Const :type)
          (->Pi "_"
                (->App (->ListT) "a")
                (->App (->OptionalT) "a"))))



  ListLast
  (subst [this var e]    this)
  (alphaNormalize [this] this)
  (normalize [this]      this)
  (typecheck [this]
    (->Pi "a"
          (->Const :type)
          (->Pi "_"
                (->App (->ListT) "a")
                (->App (->OptionalT) "a"))))



  ListIndexed
  (subst [this var e]    this)
  (alphaNormalize [this] this)
  (normalize [this]      this)
  (typecheck [this] "TODO typecheck ListIndexed")



  ListReverse
  (subst [this var e]    this)
  (alphaNormalize [this] this)
  (normalize [this]      this)
  (typecheck [this]
    (->Pi "a"
          (->Const :type)
          (->Pi "_"
                (->App (->ListT) "a")
                (->App (->ListT) "a"))))



  OptionalT
  (subst [this var e]    this)
  (alphaNormalize [this] this)
  (normalize [this]      this)
  (typecheck [this]
    (->Pi "_" (->Const :type) (->Const :type)))



  OptionalLit
  (subst [this var e]
    (let [val?  (:val? this)
          val?' (when val? (subst val? var e))]
      (-> this
         (update :type subst var e)
         (assoc :val? val?'))))
  (alphaNormalize [{:keys [val?] :as this}]
    (-> this
       (update :type alphaNormalize)
       (assoc  :val? (when val? (alphaNormalize val?)))))
  (normalize [{:keys [val?] :as this}]
    (-> this
       (update :type normalize)
       (assoc  :val? (when val? (normalize val?)))))
  (typecheck [this] "TODO typecheck OptionalLit")



  OptionalFold
  (subst [this var e]    this)
  (alphaNormalize [this] this)
  (normalize [this]      this)
  (typecheck [this] "TODO typecheck OptionalFold")



  OptionalBuild
  (subst [this var e]    this)
  (alphaNormalize [this] this)
  (normalize [this]      this)
  (typecheck [this] "TODO typecheck OptionalBuild")



  RecordT
  (subst [this var e]
    (update this :kvs (fn [kvs] (map-vals #(subst % var e) kvs))))
  (alphaNormalize [this]
    (update this :kvs (fn [kvs] (map-vals alphaNormalize kvs))))
  (normalize [this]
    (update this :kvs (fn [kvs] (map-vals normalize kvs))))
  (typecheck [this] "TODO typecheck RecordT")



  RecordLit
  (subst [this var e]
    (update this :kvs (fn [kvs] (map-vals #(subst % var e) kvs))))
  (alphaNormalize [this]
    (update this :kvs (fn [kvs] (map-vals alphaNormalize kvs))))
  (normalize [this]
    (update this :kvs (fn [kvs] (map-vals normalize kvs))))
  (typecheck [this] "TODO typecheck RecordLit")



  UnionT
  (subst [this var e]
    (update this :kvs (fn [kvs] (map-vals #(subst % var e) kvs))))
  (alphaNormalize [this]
    (update this :kvs (fn [kvs] (map-vals alphaNormalize kvs))))
  (normalize [this]
    (update this :kvs (fn [kvs] (map-vals normalize kvs))))
  (typecheck [this] "TODO typecheck UnionT")



  UnionLit
  (subst [this var e]
    (-> this
       (update :v subst var e)
       (update :kvs (fn [kvs] (map-vals #(subst % var e) kvs)))))
  (alphaNormalize [this]
    (-> this
       (update :v alphaNormalize)
       (update :kvs (fn [kvs] (map-vals alphaNormalize kvs)))))
  (normalize [this]
    (-> this
       (update :v normalize)
       (update :kvs (fn [kvs] (map-vals normalize kvs)))))
  (typecheck [this] "TODO typecheck UnionLit")



  Combine
  (subst [this var e]
    (-> this
       (update :a subst var e)
       (update :b subst var e)))
  (alphaNormalize [this]
    (-> this
       (update :a alphaNormalize)
       (update :b alphaNormalize)))
  (normalize [{:keys [a b]}]
    (letfn [(decide [l r]
              (cond
                (and (instance? RecordLit l)
                     (empty? l))              r
                (and (instance? RecordLit r)
                     (empty? r))              l
                (and (instance? RecordLit l)
                     (instance? RecordLit r)) (->RecordLit (merge-with decide (:kvs l) (:kvs r)))
                :else                         (->Combine l r)))]
      (decide (normalize a) (normalize b))))
  (typecheck [this] "TODO typecheck Combine")



  CombineTypes
  (subst [this var e]
    (-> this
       (update :a subst var e)
       (update :b subst var e)))
  (alphaNormalize [this]
    (-> this
       (update :a alphaNormalize)
       (update :b alphaNormalize)))
  (normalize [{:keys [a b]}]
    (letfn [(decide [l r]
              (cond
                (and (instance? RecordT l)
                     (empty? l))            r
                (and (instance? RecordT r)
                     (empty? r))            l
                (and (instance? RecordT l)
                     (instance? RecordT r)) (->RecordT (merge-with decide (:kvs l) (:kvs r)))
                :else                       (->CombineTypes l r)))]
      (decide (normalize a) (normalize b))))
  (typecheck [this] "TODO typecheck CombineTypes")



  Prefer
  (subst [this var e]
    (-> this
       (update :a subst var e)
       (update :b subst var e)))
  (alphaNormalize [this]
    (-> this
       (update :a alphaNormalize)
       (update :b alphaNormalize)))
  (normalize [{:keys [a b]}]
    (letfn [(decide [l r]
              (cond
                (and (instance? RecordLit l)
                     (empty? l))              r
                (and (instance? RecordLit r)
                     (empty? r))              l
                (and (instance? RecordLit l)
                     (instance? RecordLit r)) (->RecordLit (merge (:kvs l) (:kvs r)))
                :else                         (->Prefer l r)))]
      (decide (normalize a) (normalize b))))
  (typecheck [this] "TODO typecheck Prefer")



  Merge
  (subst [this var e]
    (let [type?  (:type? this)
          type?' (when type? (subst type? var e))]
      (-> this
         (update :a subst var e)
         (update :b subst var e)
         (assoc :type? type?'))))
  (alphaNormalize [{:keys [type?] :as this}]
    (-> this
       (update :a alphaNormalize)
       (update :b alphaNormalize)
       (assoc :type? (when type? (alphaNormalize type?)))))
  (normalize [{:keys [a b type?]}]
    (let [a'     (normalize a)
          b'     (normalize b)
          type?' (when type? (normalize type?))]
      (if (and (instance? RecordLit a')
               (instance? UnionLit b')
               (contains? (:kvs a') (:k b')))
        (normalize (->App (get (:kvs a') (:k b'))
                          (:v b')))
        (->Merge a' b' type?'))))
  (typecheck [this] "TODO typecheck Merge")



  Constructors
  (subst [this var e]
    (update this :e subst var e))
  (alphaNormalize [this]
    (update this :e alphaNormalize))
  (normalize [this]
    (let [e' (normalize (:e this))]
      (if (instance? UnionT e')
        (let [kts   (:kvs e')
              adapt (fn [[k v]]
                      (->Lam k v (->UnionLit k
                                             (->Var k 0)
                                             (dissoc kts k))))]
          (->RecordLit (map adapt kts)))
        (->Constructors e'))))
  (typecheck [this] "TODO typecheck Constructors")



  Field
  (subst [this var e]
    (update this :e subst var e))
  (alphaNormalize [this]
    (update this :e alphaNormalize))
  (normalize [{:keys [e k] :as this}]
    (let [e' (normalize e)]
      (if (instance? RecordLit e')
        (if-let [v (get (:kvs e') k)]
          (normalize v)
          (->Field (->RecordLit (map-vals normalize (:kvs e'))) k))
        (assoc this :e e'))))
  (typecheck [this] "TODO typecheck Field")



  Project
  (subst [this var e]
    (update this :e subst var e))
  (alphaNormalize [this]
    (update this :e alphaNormalize))
  (normalize [{:keys [e ks] :as this}]
    (let [e' (normalize e)]
      (if (instance? RecordLit e')
        (let [kvs (:kvs e')]
          (if (every? (fn [k] (contains? kvs k)) ks)
            (normalize (->RecordLit (select-keys kvs ks)))
            (->Project (->RecordLit (map-vals normalize kvs)) ks)))
        (assoc this :e e'))))
  (typecheck [this] "TODO typecheck Project")



  ImportAlt
  (subst [this var e]
    (-> this
       (update :a subst var e)
       (update :b subst var e)))
  (alphaNormalize [this]
    (-> this
       (update :a alphaNormalize)
       (update :b alphaNormalize)))
  (normalize [this]
    (normalize (:a this)))
  (typecheck [this] "TODO typecheck ImportAlt"))
