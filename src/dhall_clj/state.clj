(ns dhall-clj.state
  (:require [dhall-clj.ast :refer :all]
            [me.raynes.fs :as fs]
            [clojure.string :as str]))

(defn new
  "Create a new `State` map, to hold all the contexts and caches while we
  resolve imports and typecheck."
  []
  ;; FIXME: we should remove this "fake" import as it's totally useless by now
  (let [root-import (map->Import
                      {:mode :code
                       :data (->Local "." '() ".")})]
    {;; List of parent `Import`s that the current import descends from
     :stack     (list root-import) ;; :: List ImportData

     ;; Caches where we keep imports so we import them once and only once
     ;; The `:cache-raw` stores just resolved imports that might contain
     ;; more imports, while `:cache` stores fully normalized exprs
     :cache     (atom {})          ;; :: IORef (Map Import Expr)
     :cache-raw (atom {})          ;; :: IORef (Map Import Expr)

     ;; Typechecking context, holds what symbols we have so far
     :context   {}}))              ;; :: Map String Expr

(defmacro with-cache!
  "Given an `atom` that contains a Map, we atomically:
  - check if the key `k` is in the map
  - if it's there we return it
  - if not, we compute the value with `body`, and add it to the cache"
  [atom k body]
  `(get
     (swap!
       ~atom
       (fn [a#]
         (if (get a# ~k)
           a#
           (assoc a# ~k ~body))))
     ~k))
