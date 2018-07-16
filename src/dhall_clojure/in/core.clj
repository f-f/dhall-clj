(ns dhall-clojure.in.core
  (:require [medley.core :refer [map-vals]]))


(defprotocol Expr
  "Interface that every Expression type should implement"
  (shift [this diff var]
    "`shift` is used by both normalization and type-checking to avoid variable
    capture by shifting variable indices.
    `(shift e diff {:keys [i x]})` modifies the expression `e` by adding `diff`
    to the indices of all variables named `x` whose indices are greater than
    `(+ n m)`, where `m` is the number of bound variables of the same name
    within that scope.
    `diff` is always +1 or -1, because we either:
    * increment variables by `1` to avoid variable capture during substitution
    * decrement variables by `1` when deleting lambdas after substitution")
;;  (alphaNormalize [this]
;;    "α-normalize an expression by renaming all variables to `_` and using
;;    De Bruijn indices to distinguish them")
  (typecheck [this]
    "Typecheck the expression. Returns the type on success, or excepts"))


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


(declare
  ->IntegerT
  ->TextT
  ->DoubleT
  ->OptionalT)


(defrecord Const [c]
  Expr
  (shift [this diff var] this)
  (typecheck [this] "TODO typecheck Const"))


(defrecord Var [x i]
  Expr
  (shift [this diff {:keys [x i] :as var}]
    (let [x'  (:x this)
          i'  (:i this)
          i'' (if (and (= x x') (<= i i'))
                (+ i' diff)
                i')]
      (assoc this :i i'')))
  (typecheck [this] "TODO typecheck Var"))


(defrecord Lam [arg type body]
  Expr
  (shift [{:keys [arg type body]} diff {:keys [x i] :as var}]
    (let [i' (if (= x arg)
               (inc i)
               i)
          type' (shift type diff var)
          body' (shift body diff (->Var x i'))]
      (->Lam arg type' body')))
  (typecheck [this] "TODO typecheck Lam"))


(defrecord Pi [arg type body]
  Expr
  (shift [{:keys [arg type body]} diff {:keys [x i] :as var}]
       (let [i' (if (= x arg)
                  (inc i)
                  i)
             type' (shift type diff var)
             body' (shift body diff (->Var x i'))]
         (->Pi arg type' body')))
  (typecheck [this] "TODO typecheck Lam"))


(defrecord App [a b]
  Expr
  (shift [{:keys [a b]} diff var]
    (->App (shift a diff var)
           (shift b diff var)))
  (typecheck [this] "TODO typecheck App"))


(defrecord Let [label type? body next]
  Expr
  (shift [{:keys [label type? body next]} diff {:keys [x i] :as var}]
    (let [i' (if (= x label)
               (inc i)
               i)
          type?' (when type? (shift type? diff var))
          body'  (shift body diff var)
          next'  (shift next diff (->Var x i'))]
      (->Let label type?' body' next')))
  (typecheck [this] "TODO typecheck Let"))


(defrecord Annot [val type]
  Expr
  (shift [{:keys [val type]} diff var]
    (->Annot (shift val diff var)
             (shift type diff var)))
  (typecheck [this] "TODO typecheck Annot"))


(defrecord BoolT []
  Expr
  (shift [this diff var] this)
  (typecheck [this]
    (->Const :type)))


(defrecord BoolLit [b]
  Expr
  (shift [this diff var] this)
  (typecheck [this]
    (->BoolT)))


(defrecord BoolAnd [a b]
  Expr
  (shift [this diff var]
    (-> this
       (update :a shift diff var)
       (update :b shift diff var)))
  (typecheck [this] "TODO typecheck BoolAnd"))


(defrecord BoolOr [a b]
  Expr
  (shift [this diff var]
    (-> this
       (update :a shift diff var)
       (update :b shift diff var)))
  (typecheck [this] "TODO typecheck BoolOr"))


(defrecord BoolEQ [a b]
  Expr
  (shift [this diff var]
    (-> this
       (update :a shift diff var)
       (update :b shift diff var)))
  (typecheck [this] "TODO typecheck BoolEQ"))


(defrecord BoolNE [a b]
  Expr
  (shift [this diff var]
    (-> this
       (update :a shift diff var)
       (update :b shift diff var)))
  (typecheck [this] "TODO typecheck BoolNE"))


(defrecord BoolIf [test then else]
  Expr
  (shift [this diff var]
    (-> this
       (update :test shift diff var)
       (update :then shift diff var)
       (update :else shift diff var)))
  (typecheck [this] "TODO typecheck BoolIf"))


(defrecord NaturalT []
  Expr
  (shift [this diff var] this)
  (typecheck [this]
    (->Const :type)))


(defrecord NaturalLit [n]
  Expr
  (shift [this diff var] this)
  (typecheck [this]
    (->NaturalT)))


(defrecord NaturalFold []
  Expr
  (shift [this diff var] this)
  (typecheck [this] "TODO typecheck NaturalFold"))


(defrecord NaturalBuild []
  Expr
  (shift [this diff var] this)
  (typecheck [this] "TODO typecheck NaturalBuild"))


(defrecord NaturalIsZero []
  Expr
  (shift [this diff var] this)
  (typecheck [this]
    (->Pi "_" (->NaturalT) (->BoolT))))


(defrecord NaturalEven []
  Expr
  (shift [this diff var] this)
  (typecheck [this]
    (->Pi "_" (->NaturalT) (->BoolT))))


(defrecord NaturalOdd []
  Expr
  (shift [this diff var] this)
  (typecheck [this]
    (->Pi "_" (->NaturalT) (->BoolT))))


(defrecord NaturalToInteger []
  Expr
  (shift [this diff var] this)
  (typecheck [this]
    (->Pi "_" (->NaturalT) (->IntegerT))))


(defrecord NaturalShow []
  Expr
  (shift [this diff var] this)
  (typecheck [this]
    (->Pi "_" (->NaturalT) (->TextT))))


(defrecord NaturalPlus [a b]
  Expr
  (shift [this diff var]
    (-> this
       (update :a shift diff var)
       (update :b shift diff var)))
  (typecheck [this] "TODO typecheck NaturalPlus"))


(defrecord NaturalTimes [a b]
  Expr
  (shift [this diff var]
    (-> this
       (update :a shift diff var)
       (update :b shift diff var)))
  (typecheck [this] "TODO typecheck NaturalTimes"))


(defrecord IntegerT []
  Expr
  (shift [this diff var] this)
  (typecheck [this]
    (->Const :type)))


(defrecord IntegerLit [n]
  Expr
  (shift [this diff var] this)
  (typecheck [this]
    (->IntegerT)))


(defrecord IntegerShow []
  Expr
  (shift [this diff var] this)
  (typecheck [this]
    (->Pi "_" (->IntegerT) (->TextT))))


(defrecord IntegerToDouble []
  Expr
  (shift [this diff var] this)
  (typecheck [this]
    (->Pi "_" (->IntegerT) (->DoubleT))))


(defrecord DoubleT []
  Expr
  (shift [this diff var] this)
  (typecheck [this]
    (->Const :type)))


(defrecord DoubleLit [n]
  Expr
  (shift [this diff var] this)
  (typecheck [this]
    (->DoubleT)))


(defrecord DoubleShow []
  Expr
  (shift [this diff var] this)
  (typecheck [this]
    (->Pi "_" (->DoubleT) (->TextT))))


(defrecord TextT []
  Expr
  (shift [this diff var] this)
  (typecheck [this]
    (->Const :type)))


(defrecord TextLit [chunks]
  Expr
  (shift [this diff var]
    (let [shift-text (fn [c]
                       (if (string? c)
                         c
                         (shift c diff var)))]
      (update this :chunks #(map shift-text %))))
  (typecheck [this] "TODO typecheck TextLit"))


(defrecord TextAppend [a b]
  Expr
  (shift [this diff var]
    (-> this
       (update :a shift diff var)
       (update :b shift diff var)))
  (typecheck [this] "TODO typecheck TextAppend"))


(defrecord ListT []
  Expr
  (shift [this diff var] this)
  (typecheck [this]
    (->Pi "_" (->Const :type) (->Const :type))))


(defrecord ListLit [type? exprs]
  Expr
  (shift [this diff var]
    (let [type?  (:type? this)
          type?' (when type? (shift type? diff var))
          exprs' (mapv #(shift % diff var) exprs)]
      (-> this
         (assoc :type? type?')
         (assoc :exprs exprs'))))
  (typecheck [this] "TODO typecheck ListLit"))


(defrecord ListAppend [a b]
  Expr
  (shift [this diff var]
    (-> this
       (update :a shift diff var)
       (update :b shift diff var)))
  (typecheck [this] "TODO typecheck ListAppend"))


(defrecord ListBuild []
  Expr
  (shift [this diff var] this)
  (typecheck [this] "TODO typecheck ListBuild"))


(defrecord ListFold []
  Expr
  (shift [this diff var] this)
  (typecheck [this] "TODO typecheck ListFold"))


(defrecord ListLength []
  Expr
  (shift [this diff var] this)
  (typecheck [this]
    (->Pi "a"
          (->Const :type)
          (->Pi "_" (->App (->ListT) "a") (->NaturalT)))))


(defrecord ListHead []
  Expr
  (shift [this diff var] this)
  (typecheck [this]
    (->Pi "a"
          (->Const :type)
          (->Pi "_"
                (->App (->ListT) "a")
                (->App (->OptionalT) "a")))))


(defrecord ListLast []
  Expr
  (shift [this diff var] this)
  (typecheck [this]
    (->Pi "a"
          (->Const :type)
          (->Pi "_"
                (->App (->ListT) "a")
                (->App (->OptionalT) "a")))))


(defrecord ListIndexed []
  Expr
  (shift [this diff var] this)
  (typecheck [this] "TODO typecheck ListIndexed"))


(defrecord ListReverse []
  Expr
  (shift [this diff var] this)
  (typecheck [this]
    (->Pi "a"
          (->Const :type)
          (->Pi "_"
                (->App (->ListT) "a")
                (->App (->ListT) "a")))))


(defrecord OptionalT []
  Expr
  (shift [this diff var] this)
  (typecheck [this]
    (->Pi "_" (->Const :type) (->Const :type))))


(defrecord OptionalLit [type val?]
  Expr
  (shift [this diff var]
    (let [val?  (:val? this)
          val?' (when val? (shift val? diff var))]
      (-> this
         (update :type shift diff var)
         (assoc :val? val?'))))
  (typecheck [this] "TODO typecheck OptionalLit"))


(defrecord OptionalFold []
  Expr
  (shift [this diff var] this)
  (typecheck [this] "TODO typecheck OptionalFold"))


(defrecord OptionalBuild []
  Expr
  (shift [this diff var] this)
  (typecheck [this] "TODO typecheck OptionalBuild"))


(defrecord RecordT [kvs]
  Expr
  (shift [this diff var]
    (update this :kvs (fn [kvs] (map-vals #(shift % diff var) kvs))))
  (typecheck [this] "TODO typecheck RecordT"))


(defrecord RecordLit [kvs]
  Expr
  (shift [this diff var]
    (update this :kvs (fn [kvs] (map-vals #(shift % diff var) kvs))))
  (typecheck [this] "TODO typecheck RecordLit"))


(defrecord UnionT [kvs]
  Expr
  (shift [this diff var]
    (update this :kvs (fn [kvs] (map-vals #(shift % diff var) kvs))))
  (typecheck [this] "TODO typecheck UnionT"))


(defrecord UnionLit [k v kvs]
  Expr
  (shift [this diff var]
    (-> this
       (update :v shift diff var)
       (update :kvs (fn [kvs] (map-vals #(shift % diff var) kvs)))))
  (typecheck [this] "TODO typecheck UnionLit"))


(defrecord Combine [a b]
  Expr
  (shift [this diff var]
    (-> this
       (update :a shift diff var)
       (update :b shift diff var)))
  (typecheck [this] "TODO typecheck Combine"))


(defrecord CombineTypes [a b]
  Expr
  (shift [this diff var]
    (-> this
       (update :a shift diff var)
       (update :b shift diff var)))
  (typecheck [this] "TODO typecheck CombineTypes"))


(defrecord Prefer [a b]
  Expr
  (shift [this diff var]
    (-> this
       (update :a shift diff var)
       (update :b shift diff var)))
  (typecheck [this] "TODO typecheck Prefer"))


(defrecord Merge [a b type?]
  Expr
  (shift [this diff var]
    (let [type?  (:type? this)
          type?' (when type? (shift type? diff var))]
      (-> this
         (update :a shift diff var)
         (update :b shift diff var)
         (assoc :type? type?'))))
  (typecheck [this] "TODO typecheck Merge"))


(defrecord Constructors [e]
  Expr
  (shift [this diff var]
    (update this :e shift diff var))
  (typecheck [this] "TODO typecheck Constructors"))


(defrecord Field [e k]
  Expr
  (shift [this diff var]
    (update this :e shift diff var))
  (typecheck [this] "TODO typecheck Field"))


(defrecord Project [e ks]
  Expr
  (shift [this diff var]
    (update this :e shift diff var))
  (typecheck [this] "TODO typecheck Project"))


(defrecord ImportAlt [a b]
  Expr
  (shift [this diff var]
    (-> this
       (update :a shift diff var)
       (update :b shift diff var)))
  (typecheck [this] "TODO typecheck ImportAlt"))
