(ns dhall-clojure.in.import
  (:require [clojure.core.match :refer [match]]
            [clojure.string :as string]
            [instaparse.core :as insta]))

(defn error [context data]
  (do (println (str "Failed match: " context))
      data))

(defn new-cache
  []
  (atom {:env  {}
         :http {}
         :path {}}))

(defn cache-path [cache path]
  (let [new (swap!
              cache
              (fn [c p]
                (if (get-in c [:path path])
                  c
                  (assoc-in c [:path path] path))) ;;(slurp path))))
              path)]
    (get-in new [:path path])))

(defn process-path
  ([directory file] (process-path "" directory file))
  ([pre directory file]
   (let [parse-path-comp #(->> % nnext (map second) (concat ["/"]) (apply str))
         filename (parse-path-comp (second file))
         path (mapv parse-path-comp (rest directory))]
     (apply str (concat [pre] path [filename])))))

(defn resolve-local [cache content]
  (match (into [] content)
     [[:local-raw pre directory file] _] (cache-path cache (process-path pre directory file))
     [[:local-raw     directory file] _] (cache-path cache (process-path directory file))
     :else (error "local" content)))

(defn resolve-import [cache & content]
  (match (into [] content)
    [[:import-hashed
      [:import-type
       [typ & body]]]] (condp = typ
                         :local (resolve-local cache body)
                         :http (error "http" body)
                         :env (error "env" body))
    :else (error "import" content)))

(defn resolve-imports
  "Given an instaparse tree, resolve all the imports
  and return the new tree with the resolved trees embedded"
  [tree]
  (let [cache (new-cache)
        transform-map {:import (partial resolve-import cache)}]
    (insta/transform transform-map tree)))
