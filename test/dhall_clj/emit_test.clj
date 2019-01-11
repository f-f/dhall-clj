(ns dhall-clj.emit-test
  (:require  [clojure.test :refer :all]
             [dhall-clj.parse :refer [parse expr]]
             [dhall-clj.emit :refer [emit]]))

(def cases
  [["2.0"
    2.0]
   ["-2.0"
    -2.0]
   ["-1.0e-3"
    -0.001]
   ["1"
    1]
   ["12"
    12]
   ["0"
    0]
   ["+1"
    1]
   ["-12"
    -12]
   ["\"ABC\""
    "ABC"]
   ["\"\""
    ""]
   ["\"\\\"aaaa\""
    "\"aaaa"]
   ["\"abab ${\"1\"} cd\""
    "abab 1 cd"]
   ["''\nabcd''"
    "abcd"]
   ["''\na${\"1\"}b''"
    "a1b"]
   ["[1]"
    '(1)]
   ["[1, 2, 3]"
    '[1 2 3]]
   ["[\"A\"]"
    '("A")]
   ["[] : List Natural"
    []]
   ["[2] : List Natural"
    '(2)]
   ["[] : Optional Natural"
    nil]
   ["[2] : Optional Natural"
    2]
   ["Some 1"
    1]
   ["None Natural"
    nil]
   ["1 ? 2"
    1]
   ["{=}"
    {}]
   ["{fo-o=1, `Text`=2}"
    {"Text" 2
     "fo-o" 1}]
   ["{ _bar = 4 }"
    {"_bar" 4}]
   ["Natural/show"
    'clojure.core/str]
   ["List/fold-With"
    'List/fold-With]
   ["List/map"
    'List/map]
   ["List/map@1"
    'List/map__1]
   ["foo"
    'foo]
   ["foo@2"
    'foo__2]
   ["< Foo = 3 >"
    {"Foo" 3}]
   ["< Foo : Text | Bar = 3 | Baz : Bool >"
    {"Bar" 3}]
   ["< Foo = 2 | Bar : Bool >"
    {"Foo" 2}]
   ["(1)"
    1]
   ["λ(x : a) -> b"
    '(clojure.core/fn [x] b)]
   ["let x : t = e1 in e2"
    '(clojure.core/let [x e1] e2)]
   ["let x = e1 in e2"
    '(clojure.core/let [x e1] e2)]
   ["Natural/even 3"
    '(clojure.core/even? 3)]
   ["foo.a"
    '(clojure.core/get foo "a")]
   ["foo.{ a, b }"
    '(clojure.core/select-keys foo ["a" "b"])]
   ["foo.{a,b}.c"
    '(clojure.core/get (clojure.core/select-keys foo ["a" "b"]) "c")]
   ["True || False"
    '(clojure.core/or true false)]
   ["1 + 2"
    '(clojure.core/+ 1 2)]
   ["Integer/show +3"
    '(clojure.core/str 3)]])

(deftest emit-test
  (doseq [[in out] cases]
    (testing (str "emit: " in " => " out)
      (is (= out (-> in parse expr emit))))))
