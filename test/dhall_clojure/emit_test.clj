(ns dhall-clojure.emit-test
  (:require  [clojure.test :refer :all]))


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
    "\\\"aaaa"]
   ["\"abab ${1} cd\""
    "abab 1 cd"]
   ["''abcd''"
    "abcd"]
   ["''a${1}b''"
    "a1b"]
   ["[1]"
    '(1)]
   ["[1, 2, 3]"
    '(1 2 3)]
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
   ["1 ? 2"
    1]
   ["{=}"
    {}]
   ["{fo-o=1, `Text`=2}"
    {:Text 1
     :fo-o 2}]
   ["{ _bar = 4 }"
    {:_bar 4}]
   ["Natural/show"
    'str]
   ["List/fold-With"
    'list/fold-With]
   ["List/map"
    'list/map]
   ["List/map@1"
    'list/map_1]
   ["foo"
    'foo]
   ["foo@2"
    'foo_2]
   ["< Foo = 3 >"
    {:Foo 3}]
   ["< Foo : Text | Bar = 3 | Baz : Bool >"
    {:Bar 3}]
   ["< Foo = 2 | Bar : Bool >"
    {:Foo 2}]
   ["(1)"
    1]
   ["λ(x : a) -> b"
    '(fn [x] b)]
   ["let x : t = e1 in e2"
    '(let [x e1] e2)]
   ["let x = e1 in e2"
    '(let [x e1] e2)]
   ["Natural/even 3"
    '(even? 3)]
   ["foo.a"
    '(:a foo)]
   ["foo.{ a, b }"
    '(select-keys foo [:a :b])]
   ["foo.{a,b}.c"
    '(:c (select-keys foo [:a :b]))]
   ["True || False"
    '(or true false)]
   ["1 + 2"
    '(+ 1 2)]
   ["Integer/show +3"
    '(str 3)]])
