(ns dhall-clj.state)

(defn new
  "Create a new `State` map, to hold all the contexts and caches while we
  resolve imports and typecheck."
  []
  {;; List of `Import`s that we resolved along the way to get here
   :imported '()       ;; :: List Import
   ;; TODO: maybe we need the fake import with the (overridable) pwd here

   ;; Cache where we keep imports so we import them once and only once
   :cache    (atom {}) ;; :: Map Import Expr

   ;; Typechecking context, holds what symbols we have so far
   :context  {}})      ;; :: Map String Expr
