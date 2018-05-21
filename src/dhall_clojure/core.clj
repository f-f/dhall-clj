(ns dhall-clojure.core
  (:require [dhall-clojure.in.parse :refer [parse]]
            [dhall-clojure.in.emit :refer [emit]]))


(defn input
  "Given a spec and a Dhall expression, parse the expression
  and return a Clojure form conformed to the spec.
  When no spec is given, still runs the Dhall typechecker,
  but doesn't conform to a spec."
  ;; TODO: Handle this case
  ([expr] (input nil expr))
  ([spec expr]
   (some-> expr
           parse  ;; Dhall  -> Hiccup
           ;; TODO: resolve imports
           ;; TODO: typecheck
           ;; TODO: normalize
           emit)))  ;; Hiccup -> Clojure
