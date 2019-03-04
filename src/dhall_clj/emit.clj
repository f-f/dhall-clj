(ns dhall-clj.emit
  (:require [clojure.string :as string]
            [medley.core :refer [map-vals]]
            [dhall-clj.ast :refer :all]))


(defprotocol IEmit
  "Interface for records that can emit Clojure forms"
  (emit [e] "Returns the generated Clojure form"))


(extend-protocol IEmit

  dhall_clj.ast.Const
  (emit [{:keys [c]}]
    c)

  dhall_clj.ast.Var
  (emit [{:keys [x i]}]
    (if (zero? i)
      (symbol x)
      (symbol (str x "__" i))))

  dhall_clj.ast.Lam
  (emit [{:keys [arg type body]}]
    `(fn [~(symbol arg)]
       ~(emit body)))

  dhall_clj.ast.Pi
  (emit [{:keys [arg type body]}]
    "Pi") ;; FIXME

  dhall_clj.ast.App
  (emit [{:keys [a b]}]
    (let [f (emit a)
          v (emit b)]
      ;; Here we have to pay attention about the "magic" things, like
      ;; `None Natural` or `List a`, and type applications in general
      (cond
        ;; None $type
        (instance? dhall_clj.ast.None a) f

        :else `(~f ~v))))

  dhall_clj.ast.Let
  (emit [{:keys [bindings next]}]
    `(let ~(into
            []
            (mapcat
              (fn [{:keys [label type? e]}]
                [(symbol label) (emit e)])
              bindings))
       ~(emit next)))

  dhall_clj.ast.Annot
  (emit [{:keys [val type]}]
    (emit val)) ;; FIXME type

  dhall_clj.ast.BoolT
  (emit [this]
    "BoolT") ;; FIXME)

  dhall_clj.ast.BoolLit
  (emit [{:keys [b]}]
    b)

  dhall_clj.ast.BoolAnd
  (emit [{:keys [a b]}]
    `(and ~(emit a) ~(emit b)))

  dhall_clj.ast.BoolOr
  (emit [{:keys [a b]}]
    `(or ~(emit a) ~(emit b)))

  dhall_clj.ast.BoolEQ
  (emit [{:keys [a b]}]
    `(= ~(emit a) ~(emit b)))

  dhall_clj.ast.BoolNE
  (emit [{:keys [a b]}]
    `(not= ~(emit a) ~(emit b)))

  dhall_clj.ast.BoolIf
  (emit [{:keys [test then else]}]
    `(if ~(emit test)
       ~(emit then)
       ~(emit else)))

  dhall_clj.ast.NaturalT
  (emit [this]
    "NaturalT") ;; FIXME

  dhall_clj.ast.NaturalLit
  (emit [{:keys [n]}]
    n)

  dhall_clj.ast.NaturalFold
  (emit [this]
    this) ;; TODO

  dhall_clj.ast.NaturalBuild
  (emit [this]
    this) ;; TODO

  dhall_clj.ast.NaturalIsZero
  (emit [this]
    `zero?)

  dhall_clj.ast.NaturalEven
  (emit [this]
    `even?)

  dhall_clj.ast.NaturalOdd
  (emit [this]
    `odd?)

  dhall_clj.ast.NaturalToInteger
  (emit [this]
    `identity)

  dhall_clj.ast.NaturalShow
  (emit [this]
    `str)

  dhall_clj.ast.NaturalPlus
  (emit [{:keys [a b]}]
    `(+ ~(emit a) ~(emit b)))

  dhall_clj.ast.NaturalTimes
  (emit [{:keys [a b]}]
    `(* ~(emit a) ~(emit b)))

  dhall_clj.ast.IntegerT
  (emit [this]
    "IntegerT") ;; FIXME

  dhall_clj.ast.IntegerLit
  (emit [{:keys [n]}]
    n)

  dhall_clj.ast.IntegerShow
  (emit [this]
    `str)

  dhall_clj.ast.IntegerToDouble
  (emit [this]
    `double)

  dhall_clj.ast.DoubleT
  (emit [this]
    "DoubleT") ;; FIXME

  dhall_clj.ast.DoubleLit
  (emit [{:keys [n]}]
    n)

  dhall_clj.ast.DoubleShow
  (emit [this]
    `str)

  dhall_clj.ast.TextT
  (emit [this]
    "TextT") ;; FIXME

  dhall_clj.ast.TextLit
  (emit [{:keys [chunks]}]
    (let [chunks' (mapv
                    #(if (string? %)
                       %
                       (emit %))
                    chunks)]
      (if (every? string? chunks')
        (apply str chunks')
        `(apply str ~chunks'))))

  dhall_clj.ast.TextShow
  (emit [this]
    `str) ;; TODO

  dhall_clj.ast.TextAppend
  (emit [{:keys [a b]}]
    `(str ~(emit a) ~(emit b)))

  dhall_clj.ast.ListT
  (emit [this]
    "ListT") ;; FIXME

  dhall_clj.ast.ListLit
  (emit [{:keys [type? exprs]}]
    (mapv emit exprs))

  dhall_clj.ast.ListAppend
  (emit [{:keys [a b]}]
    `(concat ~(emit a) ~(emit b)))

  dhall_clj.ast.ListBuild
  (emit [this]
    this) ;; TODO

  dhall_clj.ast.ListFold
  (emit [this]
    this) ;; TODO

  dhall_clj.ast.ListLength
  (emit [this]
    `count)

  dhall_clj.ast.ListHead
  (emit [this]
    `first)

  dhall_clj.ast.ListLast
  (emit [this]
    `last)

  dhall_clj.ast.ListIndexed
  (emit [this]
    this) ;; TODO

  dhall_clj.ast.ListReverse
  (emit [this]
    `reverse)

  dhall_clj.ast.OptionalT
  (emit [this]
    "OptionalT") ;; FIXME

  dhall_clj.ast.OptionalLit
  (emit [{:keys [type val?]}]
    (when val?
      (emit val?))) ;; FIXME type

  dhall_clj.ast.Some
  (emit [{:keys [e]}]
    (emit e))

  dhall_clj.ast.None
  (emit [this]
    nil)

  dhall_clj.ast.OptionalFold
  (emit [this]
    this) ;; TODO

  dhall_clj.ast.OptionalBuild
  (emit [this]
    this) ;; TODO

  dhall_clj.ast.RecordT
  (emit [{:keys [kvs]}]
    "RecordT") ;; FIXME)

  dhall_clj.ast.RecordLit
  (emit [{:keys [kvs]}]
    (map-vals emit kvs))

  dhall_clj.ast.UnionT
  (emit [{:keys [kvs]}]
    "UnionT") ;; FIXME

  dhall_clj.ast.UnionLit
  (emit [{:keys [k v kvs]}]
    `{~k ~(emit v)})

  dhall_clj.ast.CombineTypes
  (emit [{:keys [a b]}]
    "CombineTypes") ;; TODO

  dhall_clj.ast.Combine
  (emit [{:keys [a b]}]
    "Combine") ;; TODO

  dhall_clj.ast.Prefer
  (emit [{:keys [a b]}]
    `(merge ~(emit a) ~(emit b)))

  dhall_clj.ast.Merge
  (emit [{:keys [a b type?]}]
    "Merge") ;; TODO

  dhall_clj.ast.Field
  (emit [{:keys [e k]}]
    `(get ~(emit e) ~k))

  dhall_clj.ast.Project
  (emit [{:keys [e ks]}]
    `(select-keys ~(emit e) ~ks))

  dhall_clj.ast.ImportAlt
  (emit [{:keys [a]}]
    (emit a)))
