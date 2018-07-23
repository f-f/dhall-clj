(ns dhall-clojure.core-test
  (:require [clojure.test :refer :all]
            [dhall-clojure.in.core :refer :all]))

(def shift-cases
  "These tests come from the `Shift` section of the standard"
  [[(shift (->Var "x" 0)
           1
           (->Var "x" 0))
    (->Var "x" 1)]
   [(shift (->Var "x" 0)
           1
           (->Var "x" 1))
    (->Var "x" 0)]
   [(shift (->Var "y" 0)
           1
           (->Var "x" 0))
    (->Var "y" 0)]
   [(shift (->Var "x" 1)
           -1
           (->Var "x" 0))
    (->Var "x" 0)]
   [(shift (->Lam "x" (->Const :type) (->Var "x" 0))
           1
           (->Var "x" 0))
    (->Lam "x" (->Const :type) (->Var "x" 0))]
   [(shift (->Pi "x" (->Const :type) (->Var "x" 0))
           1
           (->Var "x" 0))
    (->Pi "x" (->Const :type) (->Var "x" 0))]
   [(shift (->Let "x" nil (->NaturalLit 1) (->Var "x" 0))
           1
           (->Var "x" 0))
    (->Let "x" nil (->NaturalLit 1) (->Var "x" 0))]
   [(shift (->Lam "y" (->Const :type) (->Var "x" 0))
           1
           (->Var "x" 0))
    (->Lam "y" (->Const :type) (->Var "x" 1))]
   [(shift (->Pi "y" (->Const :type) (->Var "x" 0))
           1
           (->Var "x" 0))
    (->Pi "y" (->Const :type) (->Var "x" 1))]
   [(shift (->Let "y" nil (->NaturalLit 1) (->Var "x" 0))
           1
           (->Var "x" 0))
    (->Let "y" nil (->NaturalLit 1) (->Var "x" 1))]
   [(shift (->ListLit (->Var "x" 0) [])
           1
           (->Var "x" 0))
    (->ListLit (->Var "x" 1) [])]])

(deftest shift-test
  (doseq [[in out] shift-cases]
    (testing (str "Shift: " in " => " out)
      (is (= in out)))))
