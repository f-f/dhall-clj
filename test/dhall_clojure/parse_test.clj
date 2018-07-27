(ns dhall-clojure.parse-test
  (:require [clojure.test :refer :all]
            [dhall-clojure.in.core :refer :all]
            [dhall-clojure.in.import :as imp]
            [dhall-clojure.in.parse :refer [parse expr]]))

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
    (->TextLit [])]

   ["\"\\\"aaaa\""
    (->TextLit ["\\\"aaaa"])]

   ["\"abab ${1} cd\""
    (->TextLit ["abab " (->NaturalLit 1) " cd"])]

   ["''abcd''"
    (->TextLit ["abcd"])]

   ["''a${1}b''"
    (->TextLit ["a" (->NaturalLit 1) "b"])]

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
    (->RecordLit {"`Text`" (->NaturalLit 2)
                  "fo-o"   (->NaturalLit 1)})]

   ["{ _bar = 4 }"
    (->RecordLit {"_bar" (->NaturalLit 4)})]

   ["{fo-o : Natural, `Text` : Text}"
    (->RecordT {"`Text`" (->TextT)
                "fo-o"   (->NaturalT)})]

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
    (->Let "x" (->Var "t" 0) (->Var "e1" 0) (->Var "e2" 0))]

   ["let x = e1 in e2"
    (->Let "x" nil (->Var "e1" 0) (->Var "e2" 0))]

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
    (imp/map->Import {:mode :text
                      :type :env
                      :hash? "E3B0C44298FC1C149AFBF4C8996FB92427AE41E4649B934CA495991B7852B855"
                      :data (imp/->Env "TEST")})]

   ["env:\"TE\\\"ST\" sha256:e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
    (imp/map->Import {:mode :code
                      :type :env
                      :hash? "E3B0C44298FC1C149AFBF4C8996FB92427AE41E4649B934CA495991B7852B855"
                      :data (imp/->Env "TE\\\"ST")})]

   ["./relative.dhall"
    (imp/map->Import {:mode :code
                      :type :local
                      :hash? nil
                      :data (imp/map->Local {:directory '()
                                             :file "relative.dhall"
                                             :prefix "."})})]

   ["/absolute/file"
    (imp/map->Import {:mode :code
                      :type :local
                      :hash? nil
                      :data (imp/map->Local {:directory '("absolute")
                                             :file "file"
                                             :prefix nil})})]

   ["../../parent/file.dhall"
    (imp/map->Import {:mode :code
                      :type :local
                      :hash? nil
                      :data (imp/map->Local {:directory '(".." "parent")
                                             :file "file.dhall"
                                             :prefix ".."})})]

   ["~/.env"
    (imp/map->Import {:mode :code
                      :type :local
                      :hash? nil
                      :data (imp/map->Local {:directory '()
                                             :file ".env"
                                             :prefix "~"})})]

   ;; Regressions
   ["1 + 2"
    (->NaturalPlus (->NaturalLit 1) (->NaturalLit 2))]

   ["Integer/show +3"
    (->App (->IntegerShow) (->IntegerLit 3))]])


(deftest simple-input-parsing
  (doseq [[dhall tree] cases]
    (testing (str "Correct Expr Tree for Dhall expr: " dhall)
      (is (= (-> dhall parse expr) tree)))))

(def parser-suite-results
  [])  ;; TODO: implement forms and add results here

(deftest dhall-haskell-parser-suite
  (let [dhall-files (-> "dhall-haskell/tests/parser"
                       clojure.java.io/file
                       file-seq)  ;; get the list of files in the dir
        dhall-strings (->> dhall-files
                         ;; we try here because if it's a folder we cannot slurp it
                         (mapv #(try (slurp %)
                                     (catch Exception e nil)))
                         (remove nil?))]
    (doseq [[dhall clj-form] (mapv list dhall-strings parser-suite-results)]
      (testing (str "Dhall expr: " dhall)
        (let [parsed (expr (parse dhall))]
          (is (= clj-form parsed)))))))
