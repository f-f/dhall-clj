(ns dhall-clj.binary
  (:require [clojure.string :as string]
            [medley.core :refer [map-vals]]
            [dhall-clj.ast :refer :all]
            [clj-cbor.core :as cbor])
  (:import [dhall_clj.ast App]))

;; Main API

(declare cbor)
(declare decbor)

(def protocol-version "1.1")

(defn encode [e]
  (cbor/encode [protocol-version (cbor e)]))

(defn decode [e]
  (decbor (second (cbor/decode e)))) ;; TODO: this `second` is not that nice



(defn decbor
  "Given the clj-cbor representation of an expression,
  build the Dhall AST"
  [e]
  "TODO")


(defn unapply
  "When given an expression, if it's a function application (`App`)
  applied to many arguments it will flatten the application and
  return a list like `[f arg1 arg2 ..]`"
  [e]
  (if (instance? App e)
    (concat (unapply (:a e)) [(:b e)])
    [e]))

(defprotocol IBinary
  "Interface for serializing expressions in binary format"
  (cbor [e] "Transform an expression into a CBOR-ready format"))

(extend-protocol IBinary

  dhall_clj.ast.Const
  (cbor [{:keys [c]}]
    (if (= c :type)
      "Type"
      "Kind"))

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
  (cbor [{:keys [label type? body next]}]
    (if type?
      [25 label (cbor type?) (cbor body) (cbor next)]
      [25 label              (cbor body) (cbor next)]))

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
    [17 n])

  dhall_clj.ast.DoubleShow
  (cbor [this]
    "Double/show")

  dhall_clj.ast.TextT
  (cbor [this]
    "Text")

  dhall_clj.ast.TextLit
  (cbor [{:keys [chunks]}]
    (into [] (concat [18] (map #(if (string? %) % (cbor %)) chunks))))

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
    [7 (map-vals cbor kvs)])

  dhall_clj.ast.RecordLit
  (cbor [{:keys [kvs]}]
    [8 (map-vals cbor kvs)])

  dhall_clj.ast.UnionT
  (cbor [{:keys [kvs]}]
    [11 (map-vals cbor kvs)])

  dhall_clj.ast.UnionLit
  (cbor [{:keys [k v kvs]}]
    [12 k (cbor v) (map-vals cbor kvs)])

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

  dhall_clj.ast.Constructors
  (cbor [{:keys [e]}]
    [13 (cbor e)])

  dhall_clj.ast.Field
  (cbor [{:keys [e k]}]
    [9 (cbor e) k])

  dhall_clj.ast.Project
  (cbor [{:keys [e ks]}]
    (into [] (concat [10 (cbor e)] ks)))

  dhall_clj.ast.ImportAlt
  (cbor [{:keys [a b]}]
    [3 11 (cbor a) (cbor b)]))
