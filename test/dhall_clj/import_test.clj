(ns dhall-clj.import-test
  (:require  [clojure.test :refer :all]
             [medley.core :refer [map-vals]]
             [dhall-clj.ast :refer :all]
             [dhall-clj.parse :refer [parse expr]]
             [dhall-clj.import :refer [resolve-imports]]
             [dhall-clj.typecheck :refer [typecheck]]
             [dhall-clj.state :as s]
             [dhall-clj.test-utils :refer :all]
             [me.raynes.fs :as fs]))


;; Simple tests

(def success-cases
  [["./dhall-haskell/Prelude/package.dhall sha256:b575f038399d47f033b63d6e29ceb8e7778b45765778026c9015ef1d28655cc4"
    "./dhall-haskell/Prelude/package.dhall"]])

(def failure-cases
  [["./dhall-haskell/Prelude/package.dhall sha256:b575f038399d47f033b63d6e29ceb8e7778b45765778026c9015ef1d28655cc3"
    "Hash mismatch"]])

(deftest import-success-suite
  (let [import-cache (s/new)
        f #(-> %
              parse
              expr
              (resolve-imports import-cache))]
    (doseq [[actual expected] success-cases]
      (println "TESTCASE import: " actual)
      (testing actual
        (is (= (f actual) (f expected)))))))

(deftest import-failure-suite
  (let [import-cache (s/new)
        f #(-> %
              parse
              expr
              (resolve-imports import-cache))]
    (doseq [[failure message] failure-cases]
      (println "TESTCASE import: " failure)
      (testing failure
        (is (thrown-with-msg? clojure.lang.ExceptionInfo (re-pattern message)
                              (f failure)))))))
