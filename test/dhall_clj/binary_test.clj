(ns dhall-clj.binary-test
  (:require [clojure.test :as test]
            [dhall-clj.ast :refer :all]
            [dhall-clj.binary :as b]
            [clojure.test.check :as tc]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.string :as str]))

;; Utils

(def label (gen/frequency
             [[9 (gen/not-empty gen/string)]
              [1 (gen/return "_")]]))
(defn maybe [a] (gen/one-of [(gen/return nil) a]))
(defn kvs [a] (gen/resize 5 (gen/map label a)))

;; Leaves

(def natural (gen/fmap ->NaturalLit gen/nat))
(def integer (gen/fmap ->IntegerLit gen/int))
(def bool    (gen/fmap ->BoolLit gen/boolean))
(def double' (gen/fmap ->DoubleLit (gen/double* {:infinite? false :NaN? false})))
(def const   (gen/fmap ->Const (gen/elements [:kind :type :sort])))
(def var     (gen/fmap (partial apply ->Var) (gen/tuple label gen/nat)))
(def builtin (gen/elements
               [(->BoolT)
                (->NaturalT)
                (->NaturalFold)
                (->NaturalBuild)
                (->NaturalEven)
                (->NaturalOdd)
                (->NaturalIsZero)
                (->NaturalToInteger)
                (->NaturalShow)
                (->IntegerT)
                (->IntegerShow)
                (->IntegerToDouble)
                (->DoubleT)
                (->DoubleShow)
                (->TextT)
                (->ListT)
                (->ListBuild)
                (->ListFold)
                (->ListLength)
                (->ListHead)
                (->ListLast)
                (->ListIndexed)
                (->ListReverse)
                (->OptionalT)
                (->None)
                (->OptionalFold)
                (->OptionalBuild)]))

(def leaf (gen/one-of [natural integer bool double' const var builtin]))


;; Nodes that  can contain other nodes

(defn lam          [a b] (gen/fmap (partial apply ->Lam) (gen/tuple label a b)))
(defn pi           [a b] (gen/fmap (partial apply ->Pi) (gen/tuple label a b)))
(defn let'         [a b] (gen/fmap (partial apply ->Let) (gen/tuple label (maybe a) a b)))
(defn bool-if      [a b] (gen/fmap (partial apply ->BoolIf) (gen/tuple a a b)))
(defn text         [a b] (gen/fmap
                           ->TextLit
                           (gen/let [a-val a
                                     b-val b
                                     [s1 s2 s3] (gen/tuple gen/string gen/string gen/string)]
                             (gen/elements [[s1] [s1 a-val s2] [s1 a-val s2 b-val s3]]))))
(defn list'        [a b] (gen/fmap
                           (partial apply ->ListLit)
                           (gen/one-of
                             [(gen/tuple (gen/return nil) (gen/not-empty (gen/resize 5 (gen/vector b))))
                              (gen/tuple a (gen/return []))])))
(defn optional     [a b] (gen/fmap (partial apply ->OptionalLit) (gen/tuple a (maybe b))))
(defn some'        [a] (gen/fmap ->Some a))
(defn record-type  [a] (gen/fmap ->RecordT (kvs a)))
(defn record       [a] (gen/fmap ->RecordLit (kvs a)))
(defn union-type   [a] (gen/fmap ->UnionT (kvs a)))
(defn union        [a b] (gen/fmap (partial apply ->UnionLit) (gen/tuple label a (kvs b))))
(defn merge'       [a b] (gen/fmap (partial apply ->Merge) (gen/tuple a b (maybe b))))
(defn constructors [a] (gen/fmap ->Constructors a))
(defn field        [a] (gen/fmap (partial apply ->Field) (gen/tuple a label)))
(defn project      [a] (gen/fmap (partial apply ->Project) (gen/tuple a (gen/not-empty (gen/vector label)))))


(defmacro defopgen
  [op]
  (let [fn-name (name op)
        fn-sym (symbol fn-name)
        sym (symbol (str "->"
                         (condp = fn-name
                           "bool-eq" "BoolEQ"
                           "bool-ne" "BoolNE"
                           (str/join (map str/capitalize (str/split fn-name #"-"))))))]
    `(defn ~fn-sym [a# b#]
       (gen/fmap (partial apply ~sym)
                 (gen/tuple a# b#)))))

(defopgen app)
(defopgen annot)
(defopgen bool-and)
(defopgen bool-or)
(defopgen bool-eq)
(defopgen bool-ne)
(defopgen natural-plus)
(defopgen natural-times)
(defopgen text-append)
(defopgen list-append)
(defopgen import-alt)
(defopgen combine)
(defopgen combine-types)
(defopgen prefer)

(defn expr
  [depth]
  (if (= depth 0)
    leaf ;; If we shouldn't recur anymore, we return a leaf that doesn't contain more expr
    (gen/let [new-depth (gen/return (dec depth))
              depth-a   (gen/choose 0 new-depth)
              depth-b   (gen/return (- new-depth depth-a))]
      (let [a (gen/resize depth-a (gen/sized expr))
            b (gen/resize depth-b (gen/sized expr))]
        (gen/one-of
          [(lam a b)
           (pi a b)
           (let' a b)
           (bool-if a b)
           (text a b)
           (list' a b)
           (optional a b)
           (some' a)
           (record-type a)
           (record a)
           (union-type a)
           (union a b)
           (merge' a b)
           (constructors a)
           (field a)
           (project a)
           (app a b)
           (annot a b)
           (bool-and a b)
           (bool-or a b)
           (bool-eq a b)
           (bool-ne a b)
           (natural-plus a b)
           (natural-times a b)
           (text-append a b)
           (list-append a b)
           (import-alt a b)
           (combine a b)
           (combine-types a b)
           (prefer a b)])))))

(def encoding-roundtrips
  (prop/for-all [e (gen/sized expr)]
    (= e (-> e b/encode b/decode))))

(defspec binary-encoding-roundtrips-test 500 encoding-roundtrips)
