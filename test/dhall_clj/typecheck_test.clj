(ns dhall-clj.typecheck-test
  (:require  [clojure.test :refer :all]
             [medley.core :refer [map-vals]]
             [dhall-clj.ast :refer :all]
             [dhall-clj.in.parse :refer [parse expr]]
             [dhall-clj.in.import :refer [resolve-imports]]
             [dhall-clj.typecheck :refer [typecheck]]
             [dhall-clj.state :as s]
             [me.raynes.fs :as fs]))


;; Simple regression tests

(def typecheck-cases
  [[(-> "(\\(a : {}) -> \\(a : Natural) -> a) {=} 1"
       parse expr (typecheck {}))
    (->NaturalT)]])

(deftest typecheck-simple-test
  (doseq [[in out] typecheck-cases]
    (testing (str "typecheck: " in " => " out)
      (is (= in out)))))



;; Haskell implementation test suite

;; Credit: https://clojuredocs.org/clojure.core/tree-seq#example-54d33991e4b0e2ac61831d15
(defn list-files [basepath]
  (let [directory (clojure.java.io/file basepath)
        dir? #(.isDirectory %)]
    ;; we want only files, therefore filter items that are not directories.
    (filter (comp not dir?)
            (tree-seq dir? #(.listFiles %) directory))))

(def test-folder "dhall-haskell/tests/typecheck")

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

(deftest typecheck-suite
  (doseq [[testcase {:keys [actual expected]}] (all-testcases)]
    (let [parent (fs/parent testcase)
          run (fn []
                (fs/with-mutable-cwd
                  (fs/chdir parent)
                  (-> (->Annot
                       (-> actual parse expr)
                       (-> expected parse expr))
                     (resolve-imports (s/new))
                     (typecheck {})))
                nil)]
      (testing testcase
        (is (= nil (run)))))))
