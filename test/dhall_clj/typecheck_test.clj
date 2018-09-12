(ns dhall-clj.typecheck-test
  (:require  [clojure.test :refer :all]
             [medley.core :refer [map-vals]]
             [dhall-clj.ast :refer :all]
             [dhall-clj.parse :refer [parse expr]]
             [dhall-clj.import :refer [resolve-imports]]
             [dhall-clj.typecheck :refer [typecheck]]
             [dhall-clj.state :as s]
             [dhall-clj.test-utils :refer :all]
             [me.raynes.fs :as fs]))


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

(def test-folder "dhall-haskell/tests/typecheck")

(deftest typecheck-success-suite
  (doseq [[testcase {:keys [actual expected]}] (success-testcases test-folder)]
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

(deftest typecheck-failure-suite
  (doseq [[testcase dhall] (failure-testcases test-folder)]
    (let [parent (fs/parent testcase)
          run (fn []
                (fs/with-mutable-cwd
                  (fs/chdir parent)
                  (-> dhall
                     parse
                     expr
                     (resolve-imports (s/new))
                     (typecheck {}))))]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Typecheck error:"
                            (run))))))
