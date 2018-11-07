(ns dhall-clj.typecheck-test
  (:require  [clojure.test :refer :all]
             [medley.core :refer [map-vals]]
             [dhall-clj.ast :refer :all]
             [dhall-clj.parse :refer [parse expr]]
             [dhall-clj.import :refer [resolve-imports]]
             [dhall-clj.typecheck :refer [typecheck]]
             [dhall-clj.state :as s]
             [dhall-clj.test-utils :refer :all]
             [me.raynes.fs :as fs]
             [clojure.java.io :as io]))


;; Simple regression tests

(def typecheck-cases
  [[(-> "(\\(a : {}) -> \\(a : Natural) -> a) {=} 1"
       parse expr (typecheck {}))
    (->NaturalT)]
   [(-> "\"${Natural/show 1}.0\""
       parse expr (typecheck {}))
    (->TextT)]])


(deftest typecheck-simple-test
  (doseq [[in out] typecheck-cases]
    (testing (str "typecheck: " in " => " out)
      (is (= in out)))))


;; Haskell implementation test suite

(def test-folder "dhall-lang/tests/typecheck")

(def problematic
  "Here we list all the tests that blow up, so we categorize and exclude them.
  Note: they are vectors because the path creation is platform-sensitive."
  [
   ;; Waiting on issue #23
   ["dhall-lang" "tests" "typecheck" "success" "simple" "access" "1"]
   ;; Waiting on issue #17
   ["dhall-lang" "tests" "typecheck" "success" "simple" "kindParameter"]
   ["dhall-lang" "tests" "typecheck" "success" "simple" "fieldsAreTypes"]])


(defn valid-testcases []
  (let [all (success-testcases test-folder)]
    (->> problematic
       (map #(->> % (apply io/file) str))
       (apply dissoc all))))

(deftest typecheck-success-suite
  (let [import-cache (s/new)]
    (doseq [[testcase {:keys [actual expected]}] (valid-testcases)]
      (let [parent (fs/parent testcase)
            run (fn []
                  (fs/with-mutable-cwd
                    (fs/chdir parent)
                    (-> (->Annot
                         (-> actual parse expr)
                         (-> expected parse expr))
                       (resolve-imports import-cache)
                       (typecheck {})))
                  nil)]
        (println "TESTCASE" testcase)
        (testing testcase
          (is (= nil (run))))))))

(deftest typecheck-failure-suite
  (let [import-cache (s/new)]
    (doseq [[testcase dhall] (failure-testcases test-folder)]
      (let [parent (fs/parent testcase)
            run (fn []
                  (fs/with-mutable-cwd
                    (fs/chdir parent)
                    (-> dhall
                       parse
                       expr
                       (resolve-imports import-cache)
                       (typecheck {}))))]
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Typecheck error:"
                              (run)))))))
