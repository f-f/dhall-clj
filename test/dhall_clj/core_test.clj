(ns dhall-clj.core-test
  (:require  [clojure.test :refer :all]
             [dhall-clj.core :refer [input input-ast]]))


;; Simple regression tests

(def input-simple-cases
  [[((input "λ(n : Natural) → { a = \"${Natural/show n}.0\", b = \"foo\" }") 1)
    {"a" "1.0" "b" "foo"}]
   [(input "True && False")
    false]])


(deftest input-simple-test
  (doseq [[in out] input-simple-cases]
    (testing (str "input: " in " => " out)
      (is (= in out)))))
