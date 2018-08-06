(ns dhall-clj.beta-normalize-test
  (:require  [clojure.test :refer :all]
             [medley.core :refer [map-vals]]
             [dhall-clj.ast :refer :all]
             [dhall-clj.in.parse :refer [parse expr]]))

;; Simple tests from the spec

(def bnormalize-cases
  "These tests come from the `β-normalization` section of the standard"
  ;; (λ(x : Bool) → x == False) True ⇥ False
  [[(beta-normalize
      (->App
        (->Lam "x" (->BoolT)
               (->BoolEQ
                 (->Var "x" 0)
                 (->BoolLit false)))
        (->BoolLit true)))
    (->BoolLit false)]
   ;; List/length Natural [1, 2, 3] ⇥ 3
   [(beta-normalize
      (->App
        (->App (->ListLength) (->NaturalT))
        (->ListLit nil [(->NaturalLit 1)
                        (->NaturalLit 2)
                        (->NaturalLit 3)])))
    (->NaturalLit 3)]
   ;; List/length Integer ⇥ List/length Integer
   [(beta-normalize
      (->App (->ListLength) (->IntegerT)))
    (->App (->ListLength) (->IntegerT))]
   ;; λ(x : Integer) → List/length Integer [x, x, x] ⇥ λ(x : Integer) → 3
   [(beta-normalize
      (->Lam "x" (->IntegerT)
             (->App
               (->App (->ListLength) (->IntegerT))
               (->ListLit nil [(->Var "x" 0)
                               (->Var "x" 0)
                               (->Var "x" 0)]))))
    (->Lam "x" (->IntegerT) (->NaturalLit 3))]])


(deftest bnormalize-spec-test
  (doseq [[in out] bnormalize-cases]
    (testing (str "β-normalization: " in " => " out)
      (is (= in out)))))



;; Haskell implementation test suite

;; Credit: https://clojuredocs.org/clojure.core/tree-seq#example-54d33991e4b0e2ac61831d15
(defn list-files [basepath]
  (let [directory (clojure.java.io/file basepath)
        dir? #(.isDirectory %)]
    ;; we want only files, therefore filter items that are not directories.
    (filter (comp not dir?)
            (tree-seq dir? #(.listFiles %) directory))))


(defn list-testcases
  "
  Returns a record of records {'testcase name' {:actual Text, :expected Text}}
  They should be parsed, typechecked and normalized, and checked for equality
  "
  []
  (let [all-files (list-files "dhall-haskell/tests/normalization")
        map-of-testcases (group-by #(->> % str (drop-last 7) (apply str)) all-files)]
    (map-vals (fn [[actual expected]]
                {:actual   (slurp actual)
                 :expected (slurp expected)})
              map-of-testcases)))

#_
(deftest normalization-suite
  (doseq [[testcase {:keys [actual expected]}] (list-testcases)]
    (testing testcase
      (is (= (-> actual   parse expr beta-normalize)
             (-> expected parse expr beta-normalize))))))
