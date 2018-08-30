(ns dhall-clj.alpha-normalize-test
  (:require [clojure.test :refer :all]
            [dhall-clj.alpha-normalize :refer [alpha-normalize]]
            [dhall-clj.ast :refer :all]))

(def anormalize-cases
  "These tests come from the `α-normalization` section of the standard"
  ;;   λ(a : Type) → λ(b : Type) → λ(x : a) → λ(y : b) → x
  ;; ↦ λ(_ : Type) → λ(_ : Type) → λ(_ : _@1) → λ(_ : _@1) → _@1
  [[(alpha-normalize
      (->Lam "a"
             (->Const :type)
             (->Lam "b"
                    (->Const :type)
                    (->Lam "x"
                           (->Var "a" 0)
                           (->Lam "y"
                                  (->Var "b" 0)
                                  (->Var "x" 0))))))
    (->Lam "_"
           (->Const :type)
           (->Lam "_"
                  (->Const :type)
                  (->Lam "_"
                         (->Var "_" 1)
                         (->Lam "_"
                                (->Var "_" 1)
                                (->Var "_" 1)))))]
   ;;   λ(a : Type) → λ(b : Type) → a
   ;; ↦ λ(_ : Type) → λ(_ : Type) → _@1
   [(alpha-normalize
      (->Lam "a" (->Const :type) (->Lam "b" (->Const :type) (->Var "a" 0))))
    (->Lam "_" (->Const :type) (->Lam "_" (->Const :type) (->Var "_" 1)))]
   ;;   λ(x : Type) → _
   ;; ↦ λ(_ : Type) → _@1
   [(alpha-normalize
      (->Lam "x" (->Const :type) (->Var "_" 0)))
    (->Lam "_" (->Const :type) (->Var "_" 1))]
   ;;   λ(a : Type) → a
   ;; ↦ λ(_ : Type) → _
   [(alpha-normalize (->Lam "a" (->Const :type) (->Var "a" 0)))
    (->Lam "_" (->Const :type) (->Var "_" 0))]
   ;;   λ(a : Type) → a
   ;; ↦ λ(_ : Type) → _
   [(alpha-normalize (->Lam "b" (->Const :type) (->Var "b" 0)))
    (->Lam "_" (->Const :type) (->Var "_" 0))]
   ;;   λ(x : Type) → y
   ;; ↦ λ(_ : Type) → y
   [(alpha-normalize (->Lam "x" (->Const :type) (->Var "y" 0)))
    (->Lam "_" (->Const :type) (->Var "y" 0))]])



(deftest anormalize-test
  (doseq [[in out] anormalize-cases]
    (testing (str "α-normalization: " in " => " out)
      (is (= in out)))))
