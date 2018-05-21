(ns dhall-clojure.in.parse
  (:require [instaparse.core :as insta]
            [clojure.java.io :as io]))

(def grammar (slurp (io/resource "dhall.abnf")))

(def parse
  (insta/parser grammar
                :input-format :abnf
                :start :complete-expression
                :output-format :hiccup))
