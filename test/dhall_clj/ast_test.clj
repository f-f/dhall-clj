(ns dhall-clj.ast-test
  (:require [clojure.test :refer :all]
            [dhall-clj.ast :refer :all]))

(def shift-cases
  "These tests come from the `Shift` section of the standard"
  ;; ↑(1, x, 0, x) = x@1
  [[(shift (->Var "x" 0)
           1
           (->Var "x" 0))
    (->Var "x" 1)]
   ;; ↑(1, x, 1, x) = x
   [(shift (->Var "x" 0)
           1
           (->Var "x" 1))
    (->Var "x" 0)]
   ;; ↑(1, x, 0, y) = y
   [(shift (->Var "y" 0)
           1
           (->Var "x" 0))
    (->Var "y" 0)]
   ;; ↑(-1, x, 0, x@1) = x
   [(shift (->Var "x" 1)
           -1
           (->Var "x" 0))
    (->Var "x" 0)]
   ;; ↑(1, x, 0, λ(x : Type) → x) = λ(x : Type) → x
   [(shift (->Lam "x" (->Const :type) (->Var "x" 0))
           1
           (->Var "x" 0))
    (->Lam "x" (->Const :type) (->Var "x" 0))]
   ;; ↑(1, x, 0, ∀(x : Type) → x) = ∀(x : Type) → x
   [(shift (->Pi "x" (->Const :type) (->Var "x" 0))
           1
           (->Var "x" 0))
    (->Pi "x" (->Const :type) (->Var "x" 0))]
   ;; ↑(1, x, 0, let x = 1 in x) = let x = 1 in x
   [(shift (->Let "x" nil (->NaturalLit 1) (->Var "x" 0))
           1
           (->Var "x" 0))
    (->Let "x" nil (->NaturalLit 1) (->Var "x" 0))]
   ;; ↑(1, x, 0, λ(y : Type) → x) = λ(y : Type) → x@1
   [(shift (->Lam "y" (->Const :type) (->Var "x" 0))
           1
           (->Var "x" 0))
    (->Lam "y" (->Const :type) (->Var "x" 1))]
   ;; ↑(1, x, 0, ∀(y : Type) → x) = ∀(y : Type) → x@1
   [(shift (->Pi "y" (->Const :type) (->Var "x" 0))
           1
           (->Var "x" 0))
    (->Pi "y" (->Const :type) (->Var "x" 1))]
   ;; ↑(1, x, 0, let y = 1 in x) = let y = 1 in x@1
   [(shift (->Let "y" nil (->NaturalLit 1) (->Var "x" 0))
           1
           (->Var "x" 0))
    (->Let "y" nil (->NaturalLit 1) (->Var "x" 1))]
   ;; ↑(1, x, 0, List x) = List x@1
   [(shift (->ListLit (->Var "x" 0) [])
           1
           (->Var "x" 0))
    (->ListLit (->Var "x" 1) [])]])

(deftest shift-test
  (doseq [[in out] shift-cases]
    (testing (str "Shift: " in " => " out)
      (is (= in out)))))



(def subst-cases
  "These tests come from the `Substitution` section of the standard"
  ;; x[x ≔ Bool] = Bool
  [[(subst (->Var "x" 0)
           (->Var "x" 0)
           (->BoolT))
    (->BoolT)]
   ;; y[x ≔ Bool] = y
   [(subst (->Var "y" 0)
           (->Var "x" 0)
           (->BoolT))
    (->Var "y" 0)]
   ;; x[x@1 ≔ Bool] = x
   [(subst (->Var "x" 0)
           (->Var "x" 1)
           (->BoolT))
    (->Var "x" 0)]
   ;; (List x)[x ≔ Bool] = List Bool
   [(subst (->ListLit (->Var "x" 0) [])
           (->Var "x" 0)
           (->BoolT))
    (->ListLit (->BoolT) [])]
   ;; (λ(x : Text) → x)[x ≔ True] = λ(x : Text) → x
   [(subst (->Lam "x" (->TextT) (->Var "x" 0))
           (->Var "x" 0)
           (->BoolLit true))
    (->Lam "x" (->TextT) (->Var "x" 0))]
   ;; (λ(y : Text) → x)[x ≔ True] = λ(y : Text) → True
   [(subst (->Lam "y" (->TextT) (->Var "x" 0))
           (->Var "x" 0)
           (->BoolLit true))
    (->Lam "y" (->TextT) (->BoolLit true))]
   ;; (λ(x : Text) → x@1)[x ≔ True] = λ(x : Text) → True
   [(subst (->Lam "x" (->TextT) (->Var "x" 1))
           (->Var "x" 0)
           (->BoolLit true))
    (->Lam "x" (->TextT) (->BoolLit true))]
   ;; (λ(x : Text) → x@2)[x@1 ≔ True] = λ(x : Text) → True
   [(subst (->Lam "x" (->TextT) (->Var "x" 2))
           (->Var "x" 1)
           (->BoolLit true))
    (->Lam "x" (->TextT) (->BoolLit true))]
   ;; (λ(x : Type) → y)[y ≔ x] = λ(x : Type) → x@1
   [(subst (->Lam "x" (->Const :type) (->Var "y" 0))
           (->Var "y" 0)
           (->Var "x" 0))
    (->Lam "x" (->Const :type) (->Var "x" 1))]])

(deftest subst-test
  (doseq [[in out] subst-cases]
    (testing (str "Subst: " in " => " out)
      (is (= in out)))))
