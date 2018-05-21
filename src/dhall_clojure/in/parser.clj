(ns dhall-clojure.in.parser
  (:require [instaparse.core :as insta]
            [clojure.java.io :as io]))

(def grammar (slurp (io/resource "dhall.abnf")))

(def dhall-parser
  (insta/parser grammar
                :input-format :abnf
                :start :complete-expression
                :output-format :hiccup))
