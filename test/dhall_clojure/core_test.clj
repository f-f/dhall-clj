(ns dhall-clojure.core-test
  (:require [clojure.test :refer :all]
            [dhall-clojure.core :refer [input]]))

(def pairs
  [;; Primitive Expressions
   ;;; Double
   ["2.0"             2.0]
   ["-2.0"            -2.0]
   ;; TODO: exponent
   ;;; Natural
   ["+1"              1]
   ["+12"             12]
   ;;; Integer
   ["1"               1]
   ["-12"             -12]
   ["0"               0]
   ;;; Text
   ["\"ABC\""         "ABC"]
   ["\"\""            ""]
   ;;; Record Type or Literal
   ;;; Union Type or Literal
   ;;; Non-Empty List
   ["[1]"             '(1)]
   ["[1, 2, 3]"       '(1 2 3)]
   ["[\"A\"]"         '("A")]
   ;;; Import (TODO: all the kinds of imports)
   ;;; Identifier
   ;;; Reserved
   ;;; Expression in parens
   ;;; Debug
   ["1 || 2"          '(or 1 2)]])


(deftest simple-input-parsing
  (doseq [[dhall clj-form] pairs]
    (testing (str "Dhall expr: " dhall)
      (let [parsed (input dhall)]
        (is (= clj-form parsed))))))

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
