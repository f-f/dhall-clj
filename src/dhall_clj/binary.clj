(ns dhall-clj.binary
  (:require [clojure.string :as string]
            [medley.core :refer [map-vals]]
            [dhall-clj.ast :refer :all]
            [clojure.java.io :as io]
            [clj-cbor.core :as cbor]
            [dhall-clj.fail :as fail])
  (:import [dhall_clj.ast App]))

;; Main API

(declare cbor)
(declare decbor)

(defn encode
  "Encode `e` (which should be Dhall AST) into its binary form.
  Will return a `ByteArray`."
  [e]
  (cbor/encode (cbor e)))

(defn decode
  "Takes a bytearray `binary-expr`, and tries to decode it into Dhall AST.
  Will throw an exception of type `dhall-clj.fail/binary` on malformed input."
  [^bytes binary-expr]
  (decbor (cbor/decode binary-expr)))


;;;; CBOR -> Dhall AST

(defn- assert-len!
  "Given a seq `e`, it fails if it's shorter than `n`"
  [e n]
  (when (< (count e) n)
    (fail/vector-too-short! e n)))

(defn- assert-present!
  "Given a value `val` contained in `e`, throws if `val` is `nil`"
  [val e]
  (when-not e
    (fail/empty-val! e)))

(defn decbor
  "Given the clj-cbor representation of an expression, build the Dhall AST.
  Will throw an exception of type `:dhall-clj.fail/binary` on malformed input."
  [e]
  (cond
    (string? e)
    (condp = e
      "Natural/build"     (->NaturalBuild)
      "Natural/fold"      (->NaturalFold)
      "Natural/isZero"    (->NaturalIsZero)
      "Natural/even"      (->NaturalEven)
      "Natural/odd"       (->NaturalOdd)
      "Natural/toInteger" (->NaturalToInteger)
      "Natural/show"      (->NaturalShow)
      "Integer/toDouble"  (->IntegerToDouble)
      "Integer/show"      (->IntegerShow)
      "Double/show"       (->DoubleShow)
      "List/build"        (->ListBuild)
      "List/fold"         (->ListFold)
      "List/length"       (->ListLength)
      "List/head"         (->ListHead)
      "List/last"         (->ListLast)
      "List/indexed"      (->ListIndexed)
      "List/reverse"      (->ListReverse)
      "Optional/fold"     (->OptionalFold)
      "Optional/build"    (->OptionalBuild)
      "Text/show"         (->TextShow)
      "Bool"              (->BoolT)
      "Optional"          (->OptionalT)
      "None"              (->None)
      "Natural"           (->NaturalT)
      "Integer"           (->IntegerT)
      "Double"            (->DoubleT)
      "Text"              (->TextT)
      "List"              (->ListT)
      "Type"              (->Const :type)
      "Kind"              (->Const :kind)
      "Sort"              (->Const :sort)
      "_"                 (fail/wrong-encoding-for-var!)
      (->Var e 0)) ;; If no builtins match, then it's a variable

    (integer? e)
    (->Var "_" e)

    (boolean? e)
    (->BoolLit e)

    (double? e)
    (->DoubleLit e)

    (float? e)
    (->DoubleLit e)

    (vector? e)
    (let [;; Here we fail because if we got an array it must have at least
          ;; - the tag representing what it is
          ;; - at least one element of information
          _   (assert-len! e 2)
          tag (first e)]
      (if (string? tag)
        (->Var tag (second e))
        (condp = tag
          0  (let [_ (assert-len! e 3)
                   app (rest e)
                   f (decbor (first app))
                   args (mapv decbor (rest app))]
               (reduce ->App f args))
          1  (if (= 3 (count e))
               (let [[typ body] (rest e)]
                 (->Lam "_" (decbor typ) (decbor body)))
               (let [[arg typ body] (rest e)]
                 (assert-len! e 4)
                 (when (= arg "_")
                   (fail/fn-label-mismatch! arg e))
                 (->Lam arg (decbor typ) (decbor body))))
          2  (if (= 3 (count e))
               (let [[typ body] (rest e)]
                 (->Pi "_" (decbor typ) (decbor body)))
               (let [[arg typ body] (rest e)]
                 (assert-len! e 4)
                 (when (= arg "_")
                   (fail/fn-label-mismatch! arg e))
                 (->Pi arg (decbor typ) (decbor body))))
          3  (let [_ (assert-len! e 4)
                   [op a b] (rest e)]
               ((condp = op
                  0  ->BoolOr
                  1  ->BoolAnd
                  2  ->BoolEQ
                  3  ->BoolNE
                  4  ->NaturalPlus
                  5  ->NaturalTimes
                  6  ->TextAppend
                  7  ->ListAppend
                  8  ->Combine
                  9  ->Prefer
                  10 ->CombineTypes
                  11 ->ImportAlt)
                (decbor a)
                (decbor b)))
          4  (let [[typ & elems] (rest e)]
               (if (and (not (seq elems)) (nil? typ))
                 (fail/empty-list-must-have-type! e)
                 (->ListLit (decbor typ) (mapv decbor (or elems [])))))
          5  (if (= (count e) 3)
               (let [[typ val] (rest e)]
                 (assert-present! val e)
                 (if (nil? typ)
                   (->Some (decbor val))
                   (->OptionalLit (decbor typ) (decbor val))))
               (let [typ (second e)]
                 (assert-present! typ e)
                 (->OptionalLit (decbor typ) nil)))
          6  (let [[a b typ?] (rest e)
                   a' (decbor a)
                   b' (decbor b)]
               (assert-len! e 3)
               (if (nil? typ?)
                 (->Merge a' b' nil)
                 (->Merge a' b' (decbor typ?))))
          7  (->RecordT   (map-vals decbor (second e)))
          8  (->RecordLit (map-vals decbor (second e)))
          9  (let [[rec k] (rest e)]
               (assert-len! e 3)
               (->Field (decbor rec) k))
          10 (let [[rec & ks] (rest e)]
               (assert-len! e 3)
               (->Project (decbor rec) ks))
          11 (->UnionT (map-vals #(when-not (nil? %) (decbor %)) (second e)))
          12 (let [[k v? kvs] (rest e)]
               (assert-len! e 4)
               (->UnionLit k (when-not (nil? v?) (decbor v?)) (map-vals #(when-not (nil? %) (decbor %)) kvs)))
          14 (let [[test then else] (rest e)]
               (assert-len! e 4)
               (->BoolIf (decbor test) (decbor then) (decbor else)))
          15 (->NaturalLit (second e))
          16 (->IntegerLit (second e))
          18 (->TextLit
              ;; Here we exploit the fact that Text literals always have an odd count
              ;; and they alternate strings and exprs.
              ;; So we get the first string and then loop until we don't have any more
              ;; tuples of exprs and strings
              (let [[str1 & chunks] (rest e)]
                (loop [res [str1]
                       cs  chunks]
                  (if (seq cs)
                    (let [[e s & more] cs]
                      (recur (conj res (decbor e) s) more))
                    res))))
          ;; TODO imports
          25 (->Let
              (mapv
               (fn [[label type? body]]
                 (->Binding
                  label
                  (when-not (nil? type?) (decbor type?))
                  (decbor body)))
               (partition 3 (butlast (rest e))))
              (decbor (last e)))
          26 (let [[val typ] (rest e)]
               (assert-len! e 3)
               (->Annot (decbor val) (decbor typ))))))))


;;;; Dhall AST -> CBOR

(defn- unapply
  "When given an expression, if it's a function application (`App`)
  applied to many arguments it will flatten the application and
  return a list like `[f arg1 arg2 ..]`"
  [e]
  (if (instance? App e)
    (concat (unapply (:a e)) [(:b e)])
    [e]))

(defprotocol IBinary
  "Interface for serializing expressions in binary format"
  (cbor [e] "Transform an expression into a CBOR-ready Clojure data structure"))

(extend-protocol IBinary

  dhall_clj.ast.Const
  (cbor [{:keys [c]}]
    (condp = c
      :type "Type"
      :kind "Kind"
      :sort "Sort"))

  dhall_clj.ast.Var
  (cbor [{:keys [x i]}]
    (cond
      (= x "_") i
      (zero? i) x
      :else     [x i]))

  dhall_clj.ast.Lam
  (cbor [{:keys [arg type body]}]
    (let [type' (cbor type)
          body' (cbor body)]
      (if (= "_" arg)
        [1     type' body']
        [1 arg type' body'])))

  dhall_clj.ast.Pi
  (cbor [{:keys [arg type body]}]
    (let [type' (cbor type)
          body' (cbor body)]
      (if (= "_" arg)
        [2     type' body']
        [2 arg type' body'])))

  dhall_clj.ast.App
  (cbor [this]
    (into [] (concat [0] (map cbor (unapply this)))))

  dhall_clj.ast.Let
  (cbor [{:keys [bindings next]}]
    (letfn [(make-binding [{:keys [label type? e]}]
              [label
               (when type?
                 (cbor type?))
               (cbor e)])]
      (into [] (concat [25] (mapcat make-binding bindings) [(cbor next)]))))

  dhall_clj.ast.Annot
  (cbor [{:keys [val type]}]
    [26 (cbor val) (cbor type)])

  dhall_clj.ast.BoolT
  (cbor [this]
    "Bool")

  dhall_clj.ast.BoolLit
  (cbor [{:keys [b]}]
    b)

  dhall_clj.ast.BoolAnd
  (cbor [{:keys [a b]}]
    [3 1 (cbor a) (cbor b)])

  dhall_clj.ast.BoolOr
  (cbor [{:keys [a b]}]
    [3 0 (cbor a) (cbor b)])

  dhall_clj.ast.BoolEQ
  (cbor [{:keys [a b]}]
    [3 2 (cbor a) (cbor b)])

  dhall_clj.ast.BoolNE
  (cbor [{:keys [a b]}]
    [3 3 (cbor a) (cbor b)])

  dhall_clj.ast.BoolIf
  (cbor [{:keys [test then else]}]
    [14 (cbor test) (cbor then) (cbor else)])

  dhall_clj.ast.NaturalT
  (cbor [this]
    "Natural")

  dhall_clj.ast.NaturalLit
  (cbor [{:keys [n]}]
    [15 n])

  dhall_clj.ast.NaturalFold
  (cbor [this]
    "Natural/fold")

  dhall_clj.ast.NaturalBuild
  (cbor [this]
    "Natural/build")

  dhall_clj.ast.NaturalIsZero
  (cbor [this]
    "Natural/isZero")

  dhall_clj.ast.NaturalEven
  (cbor [this]
    "Natural/even")

  dhall_clj.ast.NaturalOdd
  (cbor [this]
    "Natural/odd")

  dhall_clj.ast.NaturalToInteger
  (cbor [this]
    "Natural/toInteger")

  dhall_clj.ast.NaturalShow
  (cbor [this]
    "Natural/show")

  dhall_clj.ast.NaturalPlus
  (cbor [{:keys [a b]}]
    [3 4 (cbor a) (cbor b)])

  dhall_clj.ast.NaturalTimes
  (cbor [{:keys [a b]}]
    [3 5 (cbor a) (cbor b)])

  dhall_clj.ast.IntegerT
  (cbor [this]
    "Integer")

  dhall_clj.ast.IntegerLit
  (cbor [{:keys [n]}]
    [16 n])

  dhall_clj.ast.IntegerShow
  (cbor [this]
    "Integer/show")

  dhall_clj.ast.IntegerToDouble
  (cbor [this]
    "Integer/toDouble")

  dhall_clj.ast.DoubleT
  (cbor [this]
    "Double")

  dhall_clj.ast.DoubleLit
  (cbor [{:keys [n]}]
    (cond
      (= n Double/POSITIVE_INFINITY) n
      (= n Double/NEGATIVE_INFINITY) n
      (= n (float n))                (float n)
      :else                          n))

  dhall_clj.ast.DoubleShow
  (cbor [this]
    "Double/show")

  dhall_clj.ast.TextT
  (cbor [this]
    "Text")

  dhall_clj.ast.TextLit
  (cbor [{:keys [chunks]}]
    (into [] (concat [18] (map #(if (string? %) % (cbor %)) chunks))))

  dhall_clj.ast.TextShow
  (cbor [this]
    "Text/show")

  dhall_clj.ast.TextAppend
  (cbor [{:keys [a b]}]
    [3 6 (cbor a) (cbor b)])

  dhall_clj.ast.ListT
  (cbor [this]
    "List")

  dhall_clj.ast.ListLit
  (cbor [{:keys [type? exprs]}]
    (if type?
      [4 (cbor type?)]
      (into [] (concat [4 nil] (mapv cbor exprs)))))

  dhall_clj.ast.ListAppend
  (cbor [{:keys [a b]}]
    [3 7 (cbor a) (cbor b)])

  dhall_clj.ast.ListBuild
  (cbor [this]
    "List/build")

  dhall_clj.ast.ListFold
  (cbor [this]
    "List/fold")

  dhall_clj.ast.ListLength
  (cbor [this]
    "List/length")

  dhall_clj.ast.ListHead
  (cbor [this]
    "List/head")

  dhall_clj.ast.ListLast
  (cbor [this]
    "List/last")

  dhall_clj.ast.ListIndexed
  (cbor [this]
    "List/indexed")

  dhall_clj.ast.ListReverse
  (cbor [this]
    "List/reverse")

  dhall_clj.ast.OptionalT
  (cbor [this]
    "Optional")

  dhall_clj.ast.OptionalLit
  (cbor [{:keys [type val?]}]
    (if-not val?
      [5 (cbor type)]
      [5 (cbor type) (cbor val?)]))

  dhall_clj.ast.None
  (cbor [this]
    "None")

  dhall_clj.ast.Some
  (cbor [{:keys [e]}]
    [5 nil (cbor e)])

  dhall_clj.ast.OptionalFold
  (cbor [this]
    "Optional/fold")

  dhall_clj.ast.OptionalBuild
  (cbor [this]
    "Optional/build")

  dhall_clj.ast.RecordT
  (cbor [{:keys [kvs]}]
    [7 (->> (map-vals cbor kvs) (into (sorted-map)))])

  dhall_clj.ast.RecordLit
  (cbor [{:keys [kvs]}]
    [8 (->> (map-vals cbor kvs) (into (sorted-map)))])

  dhall_clj.ast.UnionT
  (cbor [{:keys [kvs]}]
    [11 (->> (map-vals #(when-not (nil? %) (cbor %)) kvs) (into (sorted-map)))])

  dhall_clj.ast.UnionLit
  (cbor [{:keys [k v? kvs]}]
    [12 k (when-not (nil? v?) (cbor v?)) (->> (map-vals #(when-not (nil? %) (cbor %)) kvs) (into (sorted-map)))])

  dhall_clj.ast.CombineTypes
  (cbor [{:keys [a b]}]
    [3 10 (cbor a) (cbor b)])

  dhall_clj.ast.Combine
  (cbor [{:keys [a b]}]
    [3 8 (cbor a) (cbor b)])

  dhall_clj.ast.Prefer
  (cbor [{:keys [a b]}]
    [3 9 (cbor a) (cbor b)])

  dhall_clj.ast.Merge
  (cbor [{:keys [a b type?]}]
    (if type?
      [6 (cbor a) (cbor b) (cbor type?)]
      [6 (cbor a) (cbor b)]))

  dhall_clj.ast.Field
  (cbor [{:keys [e k]}]
    [9 (cbor e) k])

  dhall_clj.ast.Project
  (cbor [{:keys [e ks]}]
    (into [] (concat [10 (cbor e)] ks)))

  dhall_clj.ast.ImportAlt
  (cbor [{:keys [a b]}]
    [3 11 (cbor a) (cbor b)]))


;; Utils

;; From https://stackoverflow.com/questions/23018870
(defn slurp-bytes
  "Slurp the bytes from a slurpable thing"
  [x]
  (with-open [out (java.io.ByteArrayOutputStream.)]
    (clojure.java.io/copy (clojure.java.io/input-stream x) out)
    (.toByteArray out)))

(defn spit-bytes
  "Spit bytes to a file"
  [f content]
  (with-open [out (io/output-stream (io/file f))]
    (.write out content)))
