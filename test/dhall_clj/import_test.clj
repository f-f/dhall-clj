(ns dhall-clj.import-test
  (:require  [clojure.test :refer :all]
             [medley.core :refer [map-vals]]
             [dhall-clj.ast :refer :all]
             [dhall-clj.core :as core]
             [dhall-clj.parse :refer [parse expr]]
             [dhall-clj.import :refer [resolve-imports get-cached-file]]
             [dhall-clj.typecheck :refer [typecheck]]
             [dhall-clj.alpha-normalize :refer [alpha-normalize]]
             [dhall-clj.beta-normalize :refer [beta-normalize]]
             [dhall-clj.state :as s]
             [dhall-clj.test-utils :refer :all]
             [clojure.java.io :as io]
             [me.raynes.fs :as fs]))


(def prelude-hash "26e13b153cb428366610110d4d8f0c135e22b20179d5478bb16b1b83b3f2ca13")

(def simple-success-cases
  {"Prelude import with hash"
   {:actual   (str "./../../../Prelude/package.dhall sha256:" prelude-hash)
    :expected "./../../../Prelude/package.dhall"}})

(def simple-failure-cases
  {"Prelude import with hash" "./dhall-lang/Prelude/package.dhall sha256:b575f038399d47f033b63d6e29ceb8e7778b45765778026c9015ef1d28655cc3"})

(def test-folder "dhall-lang/tests/import")

(def problematic
  "Here we list all the tests that blow up, so we categorize and exclude them.
  Note: they are vectors because the path creation is platform-sensitive."
  [
   ;; Waiting on issue #26
   ["dhall-lang" "tests" "import" "success" "asText"]
   ;; Waiting on issue #34
   ["dhall-lang" "tests" "import" "failure" "referentiallyInsane.dhall"]
   ;; Waiting for proper cycle detection
   ["dhall-lang" "tests" "import" "failure" "cycle.dhall"]])


(defn valid-testcases []
  (let [all (success-testcases (str test-folder "/success"))]
    (->> problematic
       (map #(->> % (apply io/file) str))
       (apply dissoc all))))

(deftest import-success-suite
  (let [import-cache (s/new)
        parent (str test-folder "/success")
        f (fn [e]
            (fs/with-mutable-cwd
              (fs/chdir parent)
              (-> e
                 parse
                 expr
                 (resolve-imports import-cache)
                 (beta-normalize)
                 (alpha-normalize))))] ;; This last alpha-normalize is necessary so that cache works
    (doseq [[testcase {:keys [actual expected]}] (merge simple-success-cases
                                                        (valid-testcases))]
      (println "TESTCASE:" testcase)
      (testing actual
        (is (= (f actual) (f expected)))))))

(defn valid-failing-testcases []
  (let [all (failure-testcases test-folder)]
    (->> problematic
       (map #(->> % (apply io/file) str))
       (apply dissoc all))))

(deftest import-failure-suite
  (let [import-cache (s/new)
        parent (str test-folder "/failure")
        f (fn [e]
            (fs/with-mutable-cwd
              (fs/chdir parent)
              (-> e
                 parse
                 expr
                 (resolve-imports import-cache))))]
    (doseq [[testcase dhall] (merge simple-failure-cases
                                    (valid-failing-testcases))]
      (println "TESTCASE failure:" testcase)
      (testing testcase
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Import error:"
                              (f dhall)))))))


(defmacro time'
  "Evaluates expr and returns the amount of ms it took together with the evaluation"
  [expr]
  `(let [start# (. System (nanoTime))
         ret# ~expr]
     [ret# (/ (double (- (. System (nanoTime)) start#)) 1000000.0)]))

(deftest import-caching-suite
  (println "IMPORT CACHING")
  (testing "Prelude caching"
    (let [cache-file (get-cached-file prelude-hash)
          to-eval (str "./dhall-lang/Prelude/package.dhall sha256:" prelude-hash)
          _ (fs/delete cache-file)
          [pr1 time-uncached] (time' (core/input-ast to-eval))
          [pr2 time-cached]   (time' (core/input-ast to-eval))]
      (println "Time to fetch the uncached Prelude is > 1s")
      (is (> time-uncached 1000))
      (println "Time to fetch the cached Prelude is < 1s")
      (is (< time-cached) 1000)
      (println "The two Preludes are the same")
      (is (= (alpha-normalize pr1) pr2)))))
