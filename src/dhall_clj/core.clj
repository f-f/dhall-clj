(ns dhall-clj.core
  (:require [dhall-clj.in.parse :refer [parse expr]]
            [dhall-clj.ast :refer [beta-normalize]]
            [dhall-clj.in.emit :refer [emit]]))


(defn input-ast
  "Given a string containing Dhall code, parses, typechecks,
  and normalizes it, returning an AST.
  Will throw exceptions in case of failure."
  [dhall-code]
  (let [parse-tree (parse dhall-code)
        ast        (expr parse-tree)
        ;; ast        (resolve-imports ast)
        ;; type       (typecheck ast)
        ast        (beta-normalize ast)]
    ast))


(defn input
  "Given a spec and a Dhall expression, parse the expression
  and return a Clojure form conformed to the spec.
  When no spec is given, still runs the Dhall typechecker,
  but doesn't conform to a spec."
  ([dhall-code] (input nil dhall-code)) ;; TODO handle this case
  ([spec dhall-code] ;; TODO actually conform
   (emit (input-ast dhall-code))))

