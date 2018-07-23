(ns dhall-clojure.normalize-test
  (:require  [clojure.test :refer :all]
             [medley.core :refer [map-vals]]
             [dhall-clojure.in.core :refer :all]
             [dhall-clojure.in.parse :refer [parse expr]]))


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


(deftest normalization-suite
  (doseq [[testcase {:keys [actual expected]}] (list-testcases)]
    (testing testcase
      (let [actual'   (-> actual   parse expr alphaNormalize normalize)
            expected' (-> expected parse expr alphaNormalize normalize)]
        (is (= actual' expected'))))))
