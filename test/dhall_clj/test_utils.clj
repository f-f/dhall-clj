(ns dhall-clj.test-utils
  (:require [medley.core :refer [map-vals]]
            [clojure.string :as string]
            [clojure.java.io :as io]))


;; Credit: https://clojuredocs.org/clojure.core/tree-seq#example-54d33991e4b0e2ac61831d15
(defn list-files [basepath]
  (let [directory (clojure.java.io/file basepath)
        dir? #(.isDirectory %)]
    ;; we want only files, therefore filter items that are not directories.
    (filter (comp not dir?)
            (tree-seq dir? #(.listFiles %) directory))))

(defn failure-case?
  "Given a `File`, will return true if it's a failure test case.
  Note: we try to match both Windows and *nix paths."
  [file]
  (or (string/includes? (str file) "/failure/")
      (string/includes? (str file) "\\failure\\")))

(defn success-testcases
  "Returns a record of records {'testcase name' {:actual Text, :expected Text}}
  for the 'successful' test cases."
  [test-folder]
  (let [files (->> (list-files test-folder)
                 (remove failure-case?))
        map-of-testcases (group-by #(->> % str (drop-last 7) (apply str)) files)]
    (map-vals
      (fn [[actual expected]]
        {:actual   (slurp actual)
         :expected (slurp expected)})
      map-of-testcases)))

(defn failure-testcases
  "Returns a record of all testcases that should fail.
  The keys are the path to the file, and the values are the
  associated Dhall expression."
  [test-folder]
  (let [files (->> (list-files test-folder)
                 (filter failure-case?))]
    (into {} (mapv #(vector % (slurp %)) files))))
