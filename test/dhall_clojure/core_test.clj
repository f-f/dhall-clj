(ns dhall-clojure.core-test
  (:require [clojure.test :refer :all]
            [dhall-clojure.in.core :refer :all]
            [dhall-clojure.in.parse :refer [parse expr]]
            [dhall-clojure.core :refer [input]]))

(defrecord Testcase [dhall tree clojure])

(def cases
  [(Testcase. "2.0"
              (->DoubleLit 2.0)
              2.0)
   (Testcase. "-2.0"
              (->DoubleLit -2.0)
              -2.0)
   (Testcase. "-1.0e-3"
              (->DoubleLit -0.001)
              -0.001)
   (Testcase. "1"
              (->NaturalLit 1)
              1)
   (Testcase. "12"
              (->NaturalLit 12)
              12)
   (Testcase. "0"
              (->NaturalLit 0)
              0)
   (Testcase. "+1"
              (->IntegerLit 1)
              1)
   (Testcase. "-12"
              (->IntegerLit -12)
              -12)
   (Testcase. "\"ABC\""
              (->TextLit ["ABC"])
              "ABC")
   (Testcase. "\"\""
              (->TextLit [])
              "")
   (Testcase. "\"\\\"aaaa\""
              (->TextLit ["\\\"aaaa"])
              "\\\"aaaa")
   (Testcase. "\"abab ${1} cd\""
              (->TextLit ["abab " (->NaturalLit 1) " cd"])
              "abab 1 cd")
   (Testcase. "''abcd''"
              (->TextLit ["abcd"])
              "abcd")
   (Testcase. "''a${1}b''"
              (->TextLit ["a" (->NaturalLit 1) "b"])
              "a1b")
   (Testcase. "[1]"
              (->ListLit nil [(->NaturalLit 1)])
              '(1))
   (Testcase. "[1, 2, 3]"
              (->ListLit nil [(->NaturalLit 1) (->NaturalLit 2) (->NaturalLit 3)])
              '(1 2 3))
   (Testcase. "[\"A\"]"
              (->ListLit nil [(->TextLit ["A"])])
              '("A"))
   (Testcase. "[] : List Natural"
              (->ListLit (->NaturalT) [])
              [])
   (Testcase. "[] : Optional Natural"
              (->OptionalLit (->NaturalT) nil)
              nil)
   (Testcase. "[2] : Optional Natural"
              (->OptionalLit (->NaturalT) (->NaturalLit 2))
              2)
   (Testcase. "1 ? 2"
              (->ImportAlt (->NaturalLit 1) (->NaturalLit 2))
              1)
   (Testcase. "{}"
              (->RecordT {})
              {}) ;; TODO: is this the actual clj value?
   (Testcase. "{=}"
              (->RecordLit {})
              {})
   (Testcase. "{fo-o=1, `Text`=2}"
              (->RecordLit {"`Text`" (->NaturalLit 2)
                            "fo-o"   (->NaturalLit 1)})
              {:Text 1
               :fo-o 2})
   (Testcase. "{ _bar = 4 }"
              (->RecordLit {"_bar" (->NaturalLit 4)})
              {:_bar 4})
   (Testcase. "{fo-o : Natural, `Text` : Text}"
              (->RecordT {"`Text`" (->TextT)
                          "fo-o"   (->NaturalT)})
              {}) ;; TODO check this type
   (Testcase. "{ _bar : Natural }"
              (->RecordT {"_bar" (->NaturalT)})
              {}) ;; TODO check this type
   (Testcase. "Natural/show"
              (->NaturalShow)
              'str)
   (Testcase. "List/fold-With"
              (->Var "List/fold-With" 0)
              'list/fold-With)
   (Testcase. "List/map"
              (->Var "List/map" 0)
              'list/map)
   (Testcase. "List/map@1"
              (->Var "List/map" 1)
              'list/map_1)
   (Testcase. "foo"
              (->Var "foo" 0)
              'foo)
   (Testcase. "foo@2"
              (->Var "foo" 2)
              'foo_2)
   (Testcase. "< Foo : Natural >"
              (->UnionT {"Foo" (->NaturalT)})
              {}) ;; TODO check this type
   (Testcase. "< Foo = 3 >"
              (->UnionLit "Foo" (->NaturalLit 3) {})
              {:Foo 3})
   (Testcase. "< Foo : Text | Bar = 3 | Baz : Bool >"
              (->UnionLit "Bar" (->NaturalLit 3) {"Foo" (->TextT)
                                                  "Baz" (->BoolT)})
              {:Bar 3})
   (Testcase. "< Foo = 2 | Bar : Bool >"
              (->UnionLit "Foo" (->NaturalLit 2) {"Bar" (->BoolT)})
              {:Foo 2})
   (Testcase. "< Foo : Natural | Bar : Bool >"
              (->UnionT {"Foo" (->NaturalT)
                         "Bar" (->BoolT)})
              {}) ;; TODO check this type
   (Testcase. "(1)"
              (->NaturalLit 1)
              1)
   (Testcase. "λ(x : a) -> b"
              (->Lam "x" (->Var "a" 0) (->Var "b" 0))
              '(fn [x] b))
   (Testcase. "merge { Left = Natural/even, Right = λ(b : Bool) → b } < Right = True | Left : Natural >"
              (->Merge (->RecordLit {"Left" (->NaturalEven)
                                     "Right" (->Lam "b" (->BoolT) (->Var "b" 0))})
                       (->UnionLit "Right" (->BoolLit true) {"Left" (->NaturalT)})
                       nil)
              {}) ;; TODO figure out the clj
   (Testcase. "merge { Left = Natural/even, Right = λ(b : Bool) → b } < Left = 3 | Right : Bool > : Bool"
              (->Merge (->RecordLit {"Left" (->NaturalEven)
                                     "Right" (->Lam "b" (->BoolT) (->Var "b" 0))})
                       (->UnionLit "Left" (->NaturalLit 3) {"Right" (->BoolT)})
                       (->BoolT))
              {}) ;; TODO figure out the clj
   (Testcase. "True || False"
              (->BoolOr (->BoolLit true) (->BoolLit false))
              '(or true false))])


(deftest simple-input-parsing
  (doseq [{:keys [dhall tree clojure]} cases]
    (testing (str "Correct Expr Tree for Dhall expr: " dhall)
      ;(println tree)
      ;(println (-> dhall parse expr))
      (is (.equals (-> dhall parse expr) tree)))))

(def parser-suite-results
  [])  ;; TODO: implement forms and add results here

(deftest dhall-haskell-parser-suite
  (let [dhall-files (-> "dhall-haskell/tests/parser"
                       clojure.java.io/file
                       file-seq)  ;; get the list of files in the dir
        dhall-strings (->> dhall-files
                         ;; we try here because if it's a folder we cannot slurp it
                         (mapv #(try (slurp %)
                                     (catch Exception e nil)))
                         (remove nil?))]
    (doseq [[dhall clj-form] (mapv list dhall-strings parser-suite-results)]
      (testing (str "Dhall expr: " dhall)
        (let [parsed (input dhall)]
          (is (= clj-form parsed)))))))
