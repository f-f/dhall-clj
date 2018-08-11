(ns dhall-clj.in.fail
  (:require [com.gfredericks.catch-data :refer [throw-data]]
            [qbits.ex :as ex]))

;; Parsing

(ex/derive ::parsing      ::read)
(ex/derive ::ast-building ::read)

(defn parsing!
  "Throws an ex-info from a failure in parsing the string"
  [gll-failure]
  (throw-data
    "Failed to parse Dhall string"
    {:failure gll-failure
     :failure-printed (pr-str gll-failure)}))

(defn ast-building!
  "Throws an ex-info from a failed build of the AST"
  [tree]
  (throw-data
    "Failed to build the AST from the parse-tree; unmatched rule `%unmatched~s`"
    {:tree tree
     :unmatched (:t tree)
     :tree-printed (pr-str tree)}))


;; Import
(defn missing-env!
  "Throws an ex-info from a missing environment variable"
  [name]
  (throw-data
    "Missing environment variable: `%name~s`"
    {:name name}))
