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
              (->TextLit "ABC")
              "ABC")
   (Testcase. "\"\""
              (->TextLit "")
              "")
   (Testcase. "[1]"
              (->ListLit nil [(->NaturalLit 1)])
              '(1))
   (Testcase. "[1, 2, 3]"
              (->ListLit nil [(->NaturalLit 1) (->NaturalLit 2) (->NaturalLit 3)])
              '(1 2 3))
   (Testcase. "[\"A\"]"
              (->ListLit nil [(->TextLit "A")])
              '("A"))
   (Testcase. "True || False"
              (->BoolOr (->NaturalLit 1) (->NaturalLit 2))
              '(or 1 2))])


(deftest simple-input-parsing
  (doseq [{:keys [dhall tree clojure]} (take 8 cases)]
    (testing (str "Correct Expr Tree for Dhall expr: " dhall)
      (is (.equals (-> dhall parse expr) tree)))))

(def parser-suite-results
  [])  ;; TODO: implement forms and add results here

(deftest dhall-haskell-parser-suite
  (let [dhall-files (-> "dhall-haskell/tests/parser"
                       clojure.java.io/file
                       file-seq  ;; get the list of files in the dir
                       rest)     ;; we do rest here because the first element is the directory itself
        dhall-strings (mapv slurp dhall-files)]
    (doseq [[dhall clj-form] (mapv list dhall-strings parser-suite-results)]
      (testing (str "Dhall expr: " dhall)
        (let [parsed (input dhall)]
          (is (= clj-form parsed)))))))
