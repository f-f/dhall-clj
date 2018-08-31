(ns dhall-clj.in.fail
  (:require [com.gfredericks.catch-data :refer [throw-data]]
            [qbits.ex :as ex]))

;;
;; Top hierarchy
;;

(ex/derive ::read      ::dhall-clj)
(ex/derive ::imports   ::dhall-clj)
(ex/derive ::typecheck ::dhall-clj)


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
(ex/derive ::missing-file    ::imports)
(ex/derive ::missing-imports ::imports)
(ex/derive ::cyclic-import   ::imports)

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

(defn missing-file!
  [path import]
  (throw-data
    "File not found: `%path~s`"
    {:type ::missing-file
     :path path
     :import import}))

(defn missing-imports!
  "Throws an ex-info from a list of import errors"
  [errors imported]
  (throw-data
    "Got errors while resolving imports"
    {:type ::missing-imports
     :errors errors
     :imported imported}))

(defn cyclic-import!
  "Throws an ex-info on finding a cyclic import"
  [import]
  (throw-data
    "Cyclic import"
    {:type ::cyclic-import
     :import import}))


;;
;; Typecheck
;;

(defn type-error!
  "Throws an ex-info on a Typecheck issue"
  ([typ ctx ex]
   (type-error! typ ctx ex {}))
  ([typ ctx ex more-data]
   (throw-data
     "Typecheck error: `%type~s`"
     {:type typ
      :context ctx
      :expression ex
      :more-data more-data})))

(ex/derive ::untyped                ::typecheck)
(ex/derive ::unbound-variable       ::typecheck)
(ex/derive ::annot-mismatch         ::typecheck)
(ex/derive ::cant-and               ::typecheck)
(ex/derive ::cant-or                ::typecheck)
(ex/derive ::cant-eq                ::typecheck)
(ex/derive ::cant-neq               ::typecheck)
(ex/derive ::cant-add               ::typecheck)
(ex/derive ::cant-multiply          ::typecheck)
(ex/derive ::cant-text-append       ::typecheck)
(ex/derive ::cant-list-append       ::typecheck)
(ex/derive ::invalid-predicate      ::typecheck)
(ex/derive ::if-branch-must-be-term ::typecheck)
(ex/derive ::if-branch-mismatch     ::typecheck)
(ex/derive ::list-append-mismatch   ::typecheck)

(def untyped!                (partial type-error! ::untyped))
(def unbound-variable!       (partial type-error! ::unbound-variable))
(def annot-mismatch!         (partial type-error! ::annot-mismatch))
(def cant-and!               (partial type-error! ::cant-and))
(def cant-or!                (partial type-error! ::cant-or))
(def cant-eq!                (partial type-error! ::cant-eq))
(def cant-neq!               (partial type-error! ::cant-neq))
(def cant-add!               (partial type-error! ::cant-add))
(def cant-multiply!          (partial type-error! ::cant-multiply))
(def cant-text-append!       (partial type-error! ::cant-text-append))
(def cant-list-append!       (partial type-error! ::cant-list-append))
(def invalid-predicate!      (partial type-error! ::invalid-predicate))
(def if-branch-must-be-term! (partial type-error! ::if-branch-must-be-term))
(def if-branch-mismatch!     (partial type-error! ::if-branch-mismatch))
(def list-append-mismatch!   (partial type-error! ::list-append-mismatch))
