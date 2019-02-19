(ns dhall-clj.parser-test
  (:require [clojure.test :refer :all]
            [lambdaisland.uri :refer [uri]]
            [cheshire.core :as json]
            [clojure.java.io :as io]
            [dhall-clj.test-utils :refer :all]
            [dhall-clj.ast :refer :all]
            [dhall-clj.parse :refer [parse expr]]
            [dhall-clj.binary :as binary]))

(def cases
  [["2.0"
    (->DoubleLit 2.0)]

   ["-2.0"
    (->DoubleLit -2.0)]

   ["-1.0e-3"
    (->DoubleLit -0.001)]

   ["1"
    (->NaturalLit 1)]

   ["12"
    (->NaturalLit 12)]

   ["0"
    (->NaturalLit 0)]

   ["+1"
    (->IntegerLit 1)]

   ["-12"
    (->IntegerLit -12)]

   ["\"ABC\""
    (->TextLit ["ABC"])]

   ["\"\""
    (->TextLit [""])]

   ["\"\\\"aaaa\""
    (->TextLit ["\"aaaa"])]

   ["\"abab ${1} cd\""
    (->TextLit ["abab " (->NaturalLit 1) " cd"])]

   ["''\nabcd''"
    (->TextLit ["abcd"])]

   ["''\na${1}b''"
    (->TextLit ["a" (->NaturalLit 1) "b"])]

   ["''\n${Bool}${Natural}''"
    (->TextLit ["" (->BoolT) "" (->NaturalT) ""])]

   ["[1]"
    (->ListLit nil [(->NaturalLit 1)])]

   ["[1, 2, 3]"
    (->ListLit nil [(->NaturalLit 1) (->NaturalLit 2) (->NaturalLit 3)])]

   ["[\"A\"]"
    (->ListLit nil [(->TextLit ["A"])])]

   ["[] : List Natural"
    (->ListLit (->NaturalT) [])]

   ["[2] : List Natural"
    (->Annot (->ListLit nil [(->NaturalLit 2)])
             (->App (->ListT) (->NaturalT)))]

   ["[] : Optional Natural"
    (->OptionalLit (->NaturalT) nil)]

   ["[2] : Optional Natural"
    (->OptionalLit (->NaturalT) (->NaturalLit 2))]

   ["1 ? 2"
    (->ImportAlt (->NaturalLit 1) (->NaturalLit 2))]

   ["{}"
    (->RecordT {})]

   ["{=}"
    (->RecordLit {})]

   ["{fo-o=1, `Text`=2}"
    (->RecordLit {"Text" (->NaturalLit 2)
                  "fo-o" (->NaturalLit 1)})]

   ["{ _bar = 4 }"
    (->RecordLit {"_bar" (->NaturalLit 4)})]

   ["{fo-o : Natural, `Text` : Text}"
    (->RecordT {"Text" (->TextT)
                "fo-o" (->NaturalT)})]

   ["{ _bar : Natural }"
    (->RecordT {"_bar" (->NaturalT)})]

   ["Natural/show"
    (->NaturalShow)]

   ["List/fold-With"
    (->Var "List/fold-With" 0)]

   ["List/map"
    (->Var "List/map" 0)]

   ["List/map@1"
    (->Var "List/map" 1)]

   ["foo"
    (->Var "foo" 0)]

   ["foo@2"
    (->Var "foo" 2)]

   ["< Foo : Natural >"
    (->UnionT {"Foo" (->NaturalT)})]

   ["< Foo = 3 >"
    (->UnionLit "Foo" (->NaturalLit 3) {})]

   ["< Foo : Text | Bar = 3 | Baz : Bool >"
    (->UnionLit "Bar" (->NaturalLit 3) {"Foo" (->TextT)
                                        "Baz" (->BoolT)})]

   ["< Foo = 2 | Bar : Bool >"
    (->UnionLit "Foo" (->NaturalLit 2) {"Bar" (->BoolT)})]

   ["< Foo : Natural | Bar : Bool >"
    (->UnionT {"Foo" (->NaturalT)
               "Bar" (->BoolT)})]

   ["(1)"
    (->NaturalLit 1)]

   ["λ(x : a) -> b"
    (->Lam "x" (->Var "a" 0) (->Var "b" 0))]

   ["merge { Left = Natural/even, Right = λ(b : Bool) → b } < Right = True | Left : Natural >"
    (->Merge (->RecordLit {"Left" (->NaturalEven)
                           "Right" (->Lam "b" (->BoolT) (->Var "b" 0))})
             (->UnionLit "Right" (->BoolLit true) {"Left" (->NaturalT)})
             nil)]

   ["merge { Left = Natural/even, Right = λ(b : Bool) → b } < Left = 3 | Right : Bool > : Bool"
    (->Merge (->RecordLit {"Left" (->NaturalEven)
                           "Right" (->Lam "b" (->BoolT) (->Var "b" 0))})
             (->UnionLit "Left" (->NaturalLit 3) {"Right" (->BoolT)})
             (->BoolT))]

   ["let x : t = e1 in e2"
    (->Let
      [(->Binding "x" (->Var "t" 0) (->Var "e1" 0))]
      (->Var "e2" 0))]

   ["let x = e1 in e2"
    (->Let
      [(->Binding "x" nil (->Var "e1" 0))]
      (->Var "e2" 0))]

   ["forall (x : a) -> b"
    (->Pi "x" (->Var "a" 0) (->Var "b" 0))]

   ["Text -> Natural"
    (->Pi "_" (->TextT) (->NaturalT))]

   ["Natural/even 3"
    (->App (->NaturalEven) (->NaturalLit 3))]

   ["constructors < A : Bool >"
    (->Constructors (->UnionT {"A" (->BoolT)}))]

   ["constructors < A : Bool > 2"
    (->App (->Constructors (->UnionT {"A" (->BoolT)})) (->NaturalLit 2))]

   ["foo.a"
    (->Field (->Var "foo" 0) "a")]

   ["foo.{ a, b }"
    (->Project (->Var "foo" 0) ["a" "b"])]

   ["foo.{a,b}.c"
    (->Field (->Project (->Var "foo" 0) ["a" "b"]) "c")]

   ["True || False"
    (->BoolOr (->BoolLit true) (->BoolLit false))]

   ;; Imports
   ["env:TEST sha256:e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855 as Text"
    (map->Import
      {:mode :text
       :hash? "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
       :data (->Env "TEST")})]

   ["env:\"TE\\\"ST\" sha256:e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
    (map->Import
      {:mode :code
       :hash? "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
       :data (->Env "TE\\\"ST")})]

   ["./relative.dhall"
    (map->Import
      {:mode :code
       :hash? nil
       :data (map->Local
               {:directory '()
                :file "relative.dhall"
                :prefix? "."})})]

   ["/absolute/file"
    (map->Import
      {:mode :code
       :hash? nil
       :data (map->Local
               {:directory '("absolute")
                :file "file"
                :prefix? nil})})]

   ["../../parent/file.dhall"
    (map->Import
      {:mode :code
       :hash? nil
       :data (map->Local
               {:directory '("parent" "..")
                :file "file.dhall"
                :prefix? ".."})})]

   ["~/.env"
    (map->Import
      {:mode :code
       :hash? nil
       :data (map->Local
               {:directory '()
                :file ".env"
                :prefix? "~"})})]

   ["https://localhost/file using ./headers"
    (map->Import
      {:mode :code
       :hash? nil
       :data (map->Remote
               {:headers? (map->Import
                            {:data (map->Local
                                     {:directory '()
                                      :file "headers"
                                      :prefix? "."})
                             :hash? nil
                             :mode :code})
                :url (uri "https://localhost/file")})})]

   ["https://localhost/file using (./headers)"
    (map->Import
      {:mode :code
       :hash? nil
       :data (map->Remote
               {:headers? (map->Import
                            {:data (map->Local
                                     {:directory ()
                                      :file "headers"
                                      :prefix? "."})
                             :hash? nil
                             :mode :code})
                :url (uri "https://localhost/file")})})]

   ["https://localhost/file"
    (map->Import
      {:mode :code
       :hash? nil
       :data (map->Remote
               {:headers? nil
                :url (uri "https://localhost/file")})})]

   ["https://user:pass:more@localhost:8888/file?test#aaaa"
    (map->Import
      {:mode :code
       :hash? nil
       :data (map->Remote
               {:headers? nil
                :url (uri "https://user:pass:more@localhost:8888/file?test#aaaa")})})]

   ["http://user@example.com/some/file.dhall"
    (map->Import
      {:mode :code
       :hash? nil
       :data (map->Remote
               {:headers? nil
                :url (uri "http://user@example.com/some/file.dhall")})})]

   ;; Regressions
   ["1 + 2"
    (->NaturalPlus (->NaturalLit 1) (->NaturalLit 2))]

   ["Integer/show +3"
    (->App (->IntegerShow) (->IntegerLit 3))]])


(deftest simple-input-parsing
  (doseq [[dhall tree] cases]
    (testing (str "Correct Expr Tree for Dhall expr: " dhall)
      (is (= (-> dhall parse expr) tree)))))


;; dhall-lang test suite

(def test-folder "dhall-lang/tests/parser")

(def problematic
  "Here we list all the tests that blow up, so we categorize and exclude them.
  Note: they are vectors because the path creation is platform-sensitive."
  [
   ;; No CBOR for imports yet
   ["dhall-lang" "tests" "parser" "success" "collectionImportType"]
   ["dhall-lang" "tests" "parser" "success" "parenthesizeUsing"]
   ["dhall-lang" "tests" "parser" "success" "paths"]
   ["dhall-lang" "tests" "parser" "success" "urls"]
   ["dhall-lang" "tests" "parser" "success" "environmentVariables"]
   ["dhall-lang" "tests" "parser" "success" "pathTermination"]
   ["dhall-lang" "tests" "parser" "success" "importAlt"]
   ["dhall-lang" "tests" "parser" "success" "asText"]
   ["dhall-lang" "tests" "parser" "success" "unicodePaths"]
   ;; https://github.com/dhall-lang/dhall-lang/issues/373
   ["dhall-lang" "tests" "parser" "success" "operators"]
   ;; Something's broken
   ["dhall-lang" "tests" "parser" "success" "largeExpression"]

   ;; Waiting on issue #28
   ["dhall-lang" "tests" "parser" "success" "quotedPaths"]
   ;; Waiting on issue #26
   ["dhall-lang" "tests" "parser" "success" "double"]])


(defn valid-testcases []
  (let [all (success-testcases (str test-folder "/success"))]
    (->> problematic
       (map #(->> % (apply io/file) str))
       (apply dissoc all))))

(deftest parser-success-suite
  (doseq [[testcase {:keys [actual expected]}] (valid-testcases)]
    (println "TESTCASE" testcase)
    (testing testcase
      (is (= (-> expected json/parse-string)
             (-> actual parse expr binary/cbor))))))


(defn valid-failing-testcases []
  (let [all (failure-testcases (str test-folder "/failure"))]
    (->> problematic
       (map #(->> % (apply io/file) str))
       (apply dissoc all))))

(deftest parser-failure-suite
  (doseq [[testcase dhall] (valid-failing-testcases)]
    (println "TESTCASE" testcase)
    (testing testcase
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Parse error:"
                            (-> dhall parse expr binary/cbor))))))
