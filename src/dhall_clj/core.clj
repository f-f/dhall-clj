(ns dhall-clj.core
  (:require [me.raynes.fs :as fs]
            [dhall-clj.parse :refer [parse expr]]
            [dhall-clj.beta-normalize :refer [beta-normalize]]
            [dhall-clj.emit :refer [emit]]
            [dhall-clj.import :refer [resolve-imports]]
            [dhall-clj.typecheck :refer [typecheck]]
            [dhall-clj.state :as s]))


(defn input-ast
  "Given a string containing Dhall code, parses, typechecks,
  and normalizes it, returning an AST.
  Will throw exceptions in case of failure.
  See the README on how to catch them."
  ([dhall-code] (input-ast "." dhall-code))
  ([cwd dhall-code]
   (let [here (if (fs/directory? cwd)
                cwd
                ".")]
     (fs/with-mutable-cwd
       (fs/chdir here)
       (let [parse-tree (parse dhall-code)
             ast        (expr parse-tree)
             ast        (resolve-imports ast (s/new))
             type       (typecheck ast {})
             ast        (beta-normalize ast)]
         ast)))))

(defn input
  "Given a string containing a Dhall expression, parses, typechecks
  normalizes it in Dhall context, then emits and evals the Clojure form of it.
  Will except an ex-info in case of failure"
  ([dhall-code] (input "." dhall-code))
  ([cwd dhall-code]
   (-> (input-ast cwd dhall-code)
      (emit)
      (eval))))
