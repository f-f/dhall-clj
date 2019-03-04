(ns dhall-clj.ast
  (:require [medley.core :refer [map-vals]]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint]])
  (:gen-class))


;;
;; Imports classes
;;

(defrecord Import [hash?  ;; maybe a sha256 in hex
                   mode   ;; :code or :text
                   data]) ;; the actual import, records defined below

(defrecord Local [prefix? directory file])
(defrecord Remote [url headers?])
(defrecord Env [name])
(defrecord Missing [])



;; All classes that form the expression tree follow

(defrecord Const [c])
(defrecord Var [x i])
(defrecord Lam [arg type body])
(defrecord Pi [arg type body])
(defrecord App [a b])
(defrecord Binding [label type? e]) ;; This is only ever contained only in Let
(defrecord Let [bindings next])
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
(defrecord TextShow [])
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
(defrecord Some [e])
(defrecord None [])
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
(defrecord Field [e k])
(defrecord Project [e ks])
(defrecord ImportAlt [a b])


;; Utils


(defn map-chunks [this f]
  (update this :chunks #(map (fn [el] (if (string? el) el (f el))) %)))


(defn compact-chunks [chunks]
  (loop [cs  chunks
         acc nil
         new []]
    (if (seq cs)
      (let [c (first cs)]
        (if (string? c)
          (recur (rest cs)
                 (str acc c)
                 new)
          (recur (rest cs)
                 nil
                 (conj new (or acc "") c))))
      (if-not acc
        new
        (conj new acc)))))

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
  (shift [{:keys [bindings next] :as this} diff {:keys [x i] :as var}]
    (let [[{:keys [label type?] :as binding} & more-bindings] bindings
          i' (if (= x label)
               (inc i)
               i)
          type?' (when type? (shift type? diff var))
          binding' (-> binding
                      (assoc :type? type?')
                      (update :e shift diff var))]
      (if-not (seq more-bindings)
        (-> this
           (assoc :bindings (list binding'))
           (update :next shift diff (assoc var :i i')))
        (let [new (shift (update this :bindings rest) diff var)]
          (update new :bindings conj binding')))))

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

  TextShow
  (shift [this diff var] this)

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

  Some
  (shift [this diff var]
    (update this :e shift diff var))

  None
  (shift [this diff var] this)

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
  (subst [{:keys [bindings next] :as this} {:keys [x i] :as var} e]
    (let [[{:keys [label type?] :as binding} & more-bindings] bindings
          y  label
          i' (if (= x y)
               (inc i)
               i)
          type?' (when type? (subst type? var e))
          binding' (-> binding
                      (assoc :type? type?')
                      (update :e subst var e))]
      (if-not (seq more-bindings)
        (-> this
           (assoc :bindings (list binding'))
           (update :next subst (->Var x i') (shift e 1 (->Var y 0))))
        (let [new (subst (update this :bindings rest) var e)]
          (update new :bindings conj binding')))))

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

  TextShow
  (subst [this var e] this)

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

  Some
  (subst [this var e]
    (update this :e subst var e))

  None
  (subst [this var e] this)

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
