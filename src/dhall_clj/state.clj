(ns dhall-clj.state
  (:require [dhall-clj.ast :refer :all]
            [me.raynes.fs :as fs]
            [clojure.string :as str]))

(defn new
  "Create a new `State` map, to hold all the contexts and caches while we
  resolve imports and typecheck."
  [pwd]
  ;; Here we have to create a "fake import" so that we can
  ;; pass in an arbitrary filepath to use as pwd for import resolution
  (let [prefix?     (when-not (fs/absolute? pwd) ".")
        components  (str/split pwd #"/")
        file        "."
        directory   (or (reverse components) '())
        root-import (map->Import
                      {:mode :code
                       :data (->Local prefix? directory file)})]
    {;; List of parent `Import`s that the current import descends from
     :stack   (list root-import) ;; :: List ImportData

     ;; Cache where we keep imports so we import them once and only once
     :cache   (atom {})          ;; :: IORef (Map Import Expr)

     ;; Typechecking context, holds what symbols we have so far
     :context {}}))              ;; :: Map String Expr
