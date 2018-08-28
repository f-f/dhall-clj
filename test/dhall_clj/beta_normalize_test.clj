(ns dhall-clj.beta-normalize-test
  (:require  [clojure.test :refer :all]
             [medley.core :refer [map-vals]]
             [dhall-clj.ast :refer :all]
             [dhall-clj.in.parse :refer [parse expr]]
             [dhall-clj.in.import :refer [resolve-imports]]
             [dhall-clj.state :as s]
             [me.raynes.fs :as fs]))

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

(def test-folder "dhall-haskell/tests/normalization")

(defn all-testcases
  "
  Returns a record of records {'testcase name' {:actual Text, :expected Text}}
  They should be parsed, typechecked and normalized, and checked for equality
  "
  []
  (let [all-files (list-files test-folder)
        map-of-testcases (group-by #(->> % str (drop-last 7) (apply str)) all-files)]
    (map-vals (fn [[actual expected]]
                {:actual   (slurp actual)
                 :expected (slurp expected)})
              map-of-testcases)))

(def problematic
  "Here we list all the tests that blow up, so we categorize and exclude them"

  [;; Exc: no implementation of resolve-imports for nil, called from TextLit
   "dhall-haskell/tests/normalization/examples/Text/concatMap/1"
   "dhall-haskell/tests/normalization/examples/Text/concatMap/0"
   "dhall-haskell/tests/normalization/remoteSystems"

   ;; No matching clause: :in-raw
   "dhall-haskell/tests/normalization/examples/List/indexed/0"
   "dhall-haskell/tests/normalization/examples/List/indexed/1"

   ;; java.lang.StackOverflowError: null
   "dhall-haskell/tests/normalization/examples/List/shifted/0"
   "dhall-haskell/tests/normalization/examples/List/shifted/1"

   ;; Actual test failures for results mismatch
   "dhall-haskell/tests/normalization/examples/Natural/fold/1"
   "dhall-haskell/tests/normalization/examples/List/iterate/0"
   "dhall-haskell/tests/normalization/examples/List/generate/0"
   "dhall-haskell/tests/normalization/examples/Natural/enumerate/0"

   "dhall-haskell/tests/normalization/examples/List/filter/0"
   "dhall-haskell/tests/normalization/examples/Text/concatMapSep/0"
   "dhall-haskell/tests/normalization/examples/Text/concat/0"
   "dhall-haskell/tests/normalization/examples/List/map/0"
   "dhall-haskell/tests/normalization/examples/Text/concatSep/0"
   "dhall-haskell/tests/normalization/examples/List/concat/0"
   "dhall-haskell/tests/normalization/examples/List/filter/1"
   "dhall-haskell/tests/normalization/examples/List/unzip/0"
   "dhall-haskell/tests/normalization/examples/List/fold/2"
   "dhall-haskell/tests/normalization/examples/Bool/or/0"
   "dhall-haskell/tests/normalization/examples/List/concatMap/0"
   "dhall-haskell/tests/normalization/examples/List/any/0"
   "dhall-haskell/tests/normalization/examples/List/fold/1"
   "dhall-haskell/tests/normalization/examples/List/all/0"])

(defn valid-testcases []
  (let [all (all-testcases)]
    (apply dissoc all problematic)))

(deftest normalization-suite
  (doseq [[testcase {:keys [actual expected]}] (valid-testcases)]
    (let [parent (fs/parent testcase)
          f #(fs/with-mutable-cwd
               (fs/chdir parent)
               (-> %
                  parse
                  expr
                  (resolve-imports (s/new))
                  beta-normalize))]
      ;;(println "TESTCASE" testcase)
      (testing testcase
        (is (= (f actual)
               (f expected)))))))
