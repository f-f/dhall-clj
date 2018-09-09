(ns dhall-clj.context
  (:require [medley.core :refer [map-vals]]))

;; TODO document

(defn insert
  "Inserts in the `ctx` a `val` in the list for the `name`."
  [val name ctx]
  (update ctx name
          (fn [current]
            (if-not current
              (list val)
              (conj current val)))))

(defn lookup
  "Looks for the `name`'s `i`th instance in the `ctx`.
  Returns the result if it's there, or `nil` if not."
  [ctx name i]
  (try
    (nth (get ctx name) i)
    (catch Exception e
      nil)))

(defn transform
  "Given a `f :: Expr -> Expr`, traverses the whole `ctx`
  applying it to all values."
  [ctx f]
  (map-vals (partial map f) ctx))
