(ns dhall-clj.in.fail
  (:require [com.gfredericks.catch-data :refer [throw-data]]
            [qbits.ex :as ex]))

;;
;; Top hierarchy
;;

(ex/derive ::read    ::dhall-clj)
(ex/derive ::imports ::dhall-clj)


;;
;; Parsing
;;

(ex/derive ::parsing      ::read)
(ex/derive ::ast-building ::read)

(defn parsing!
  "Throws an ex-info from a failure in parsing the string"
  [gll-failure]
  (throw-data
    "Failed to parse Dhall string"
    {:type ::parsing
     :failure gll-failure
     :failure-printed (pr-str gll-failure)}))

(defn ast-building!
  "Throws an ex-info from a failed build of the AST"
  [tree]
  (throw-data
    "Failed to build the AST from the parse-tree; unmatched rule `%unmatched~s`"
    {:type ::ast-building
     :tree tree
     :unmatched (:t tree)
     :tree-printed (pr-str tree)}))


;;
;; Import
;;

(ex/derive ::missing-keyword ::imports)
(ex/derive ::missing-env     ::imports)

(defn missing-keyword!
  "Throws an ex-info from the `missing` keyword"
  []
  (throw-data
    "Found `missing` keyword"
    {:type ::missing-keyword}))

(defn missing-env!
  "Throws an ex-info from a missing environment variable"
  [name]
  (throw-data
    "Missing environment variable: `%name~s`"
    {:type ::missing-env
     :name name}))
