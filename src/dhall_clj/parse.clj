(ns dhall-clj.parse
  (:require [instaparse.core :as insta]
            [clojure.java.io :as io]
            [lambdaisland.uri :refer [uri join]]
            [dhall-clj.ast :refer :all]
            [dhall-clj.fail :as fail]
            [clojure.string :as str]))

(def grammar
  "The Dhall grammar specified in ABNF"
  (slurp (io/resource "dhall.abnf")))

(def keywords
  ["Bool"
   "Optional"
   "None"
   "Natural"
   "Integer"
   "Double"
   "Text"
   "True"
   "False"
   "NaN"
   "Infinity"
   "Type"
   "Kind"
   "Sort"
   "List"
   "Natural/fold"
   "Natural/build"
   "Natural/isZero"
   "Natural/even"
   "Natural/odd"
   "Natural/toInteger"
   "Natural/show"
   "Integer/toDouble"
   "Integer/show"
   "Double/show"
   "List/build"
   "List/fold"
   "List/length"
   "List/head"
   "List/last"
   "List/indexed"
   "List/reverse"
   "Optional/fold"
   "Optional/build"
   "Text/show"])


(defn patch-grammar [grammar]
  (let [keywords-rule (str "reserved-clj = \""
                           (str/join "\" / \"" keywords)
                           "\"")]
    (str (-> grammar
            ;; We add negative lookahead to avoid matching keywords in simple labels
            (str/replace #"simple-label = (.*)" "simple-label = keyword 1*simple-label-next-char / !keyword (simple-label-first-char *simple-label-next-char)")
            (str/replace #"nonreserved-label = (.*)" "nonreserved-label = \"`\" quoted-label \"`\" / reserved-clj 1*simple-label-next-char / !reserved-clj (simple-label-first-char *simple-label-next-char)")
            ;; Grammar apparently doesn't allow lowercase hashes
            (str/replace #"HEXDIG = " "HEXDIG = \"a\" /  \"b\" / \"c\" / \"d\" / \"e\" / \"f\" / "))
         "\n\n"
         keywords-rule)))

(def dhall-parser
  "Dhall parser generated by Instaparse from the ABNF grammar"
  (insta/parser (patch-grammar grammar)
                :input-format :abnf
                :start :complete-expression
                :output-format :enlive
                :string-ci false))

(defn clean
  "Cut the names of the attrs of the tree
  TODO: save the meta!"
  [tree]
  (if (map? tree)
    {:c (map clean (:content tree))
     ;;:a (:attrs tree)
     :t (:tag tree)}
    tree))

(defn parse
  "Parses the Dhall input, on success returns an Enlive-style tree.
  Throws on a failed parse."
  [dhall-string]
  (let [parsed (dhall-parser dhall-string)]
    (if (insta/failure? parsed)
      (fail/parsing! parsed)
      (clean parsed))))

;;
;; Utils
;;
(declare expr)

(defn first-child-expr
  "Folds the current expression into its first child"
  [e]
  (expr (-> e :c first)))

(defn children?
  "True if there is more than one child"
  [e]
  (> (count (:c e)) 1))

(defn compact
  "Given a parse tree, it will compact all the text in it,
  and return a single string"
  [tree]
  (cond
    (map? tree) (apply str (mapv compact (:c tree)))
    (string? tree) tree
    (seqable? tree) (apply str (mapv compact tree))
    :else tree))

;;
;; Parse Tree -> Expression Tree
;;

(defmulti expr
  "Takes an enlive parse tree, and constructs a tree of
  objects implementing IExpr"
  :t)


;;
;; Rules that we eliminate as not needed
;;

(defmethod expr :complete-expression [e]
  (expr (-> e :c second)))

(defmethod expr :operator-expression [e]
  (first-child-expr e))

(defmethod expr :import-expression [e]
  (first-child-expr e))

(defmethod expr :any-label [e]
  (first-child-expr e))

;;
;; Import rules
;;

(defmethod expr :import [{:keys [c]}]
  (let [import (expr (first c))
        mode (if (> (count c) 1) ;; If yes, we have a "as Text"
               ;; FIXME: other imports modes go here, maybe make it more robust
               :text
               :code)]
    (assoc import :mode mode)))

(defmethod expr :import-hashed [{:keys [c]}]
  (let [import (expr (first c))
        hash?  (when (> (count c) 1) ;; If yes, we have a hash
                 (expr (nth c 2)))]
    (assoc import :hash? hash?)))

(defmethod expr :hash [{:keys [c]}]
  (-> c
     (compact)
     (subs 7) ;; Cut the "sha256:" prefix here
     (str/trim)))

(defmethod expr :import-type [{:keys [c]}]
  (-> c first expr))

(defmethod expr :missing [_]
  (map->Import {:data (->Missing)}))

(defmethod expr :env [{:keys [c]}]
  (let [envname (compact (nth c (if (string? (second c)) 2 1)))
        env (->Env envname)]
    (map->Import {:data env})))

(defmethod expr :path-component [{:keys [c]}]
  (if (> (count (rest c)) 1)
    (-> (nth c 2) :c compact)
    (-> (second c) :c compact)))

(defmethod expr :local [{:keys [c]}]
  (let [first-tag (-> c first :t)
        [prefix? path-components]
        (case first-tag
          :parent-path   [".." (-> c first :c second :c)]
          :here-path     ["."  (-> c first :c second :c)]
          :home-path     ["~"  (-> c first :c second :c)]
          :absolute-path [nil  (-> c first :c first :c)])
        path (mapv expr path-components)
        directory (-> path butlast reverse) ;; Yes, we store the path components reversed
        file (-> path last)
        local (->Local prefix? directory file)]
    (map->Import {:data local})))

(defmethod expr :http [{:keys [c]}]
  (let [headers? (case (count c)
                   5   (-> c (nth 4) expr)
                   9   (-> c (nth 6) expr)
                   nil)
        headers? (when headers?
                   (assoc headers? :mode :code)) ;; FIXME: we should have a smart constructor
        remote (map->Remote
                 {:url      (-> c first expr)
                  :headers? headers?})]
    (map->Import {:data remote})))

(defmethod expr :http-raw [{:keys [c]}]
  (let [compact-next (fn [char coll]
                       (some->>
                         coll
                         (partition 2 1)
                         (filter (fn [[a b]] (= char a)))
                         (first)
                         (second)
                         (compact)))
        base     (uri "")
        scheme   (-> c first compact)
        query    (compact-next "?" c)

        host      (-> c (nth 2) :c)
        port      (compact-next ":" host)
        userinfo? (= "@" (second host))
        authority (-> host
                     (nth (if userinfo? 2 0))
                     compact)
        [user password] (when userinfo?
                          ((juxt (partial take-while (partial not= ":"))
                                 (partial drop-while (partial not= ":")))
                           (-> host first :c)))
        user     (when (seq user)     (compact user))
        password (when (seq password) (compact (rest password)))
        path (->> (nth c 3) :c (mapv expr))]
    (assoc base
           :scheme scheme
           :host authority
           :user user
           :password password
           :path path
           :port port
           :query query)))


;;
;; Useful rules that parse the pure subset of the language
;;

(defmethod expr :let-binding [{:keys [c]}]
  (if-let [type? (when (= ":" (nth c 4))
                   (expr (nth c 6)))]
    (->Binding
     (expr (nth c 2))
     type?
     (expr (nth c 10)))
    (->Binding
     (expr (nth c 2))
     nil
     (expr (nth c 6)))))

(defmethod expr :expression [{:keys [c t]}]
  (let [first-tag (or (-> c first :t) (first c))]
    (case first-tag
      :lambda (->Lam (expr (nth c 4))
                     (expr (nth c 8))
                     (expr (nth c 14)))
      :if     (->BoolIf (expr (nth c 2))
                        (expr (nth c 6))
                        (expr (nth c 10)))
      :let-binding (->Let (mapv expr (drop-last 3 c)) (expr (last c)))
      :forall (->Pi (expr (nth c 4))
                    (expr (nth c 8))
                    (expr (nth c 14)))
      :operator-expression  (->Pi "_"
                                  (expr (nth c 0))
                                  (expr (nth c 4)))
      :merge               (->Merge
                            (-> c (nth 2) expr)
                            (-> c (nth 4) expr)
                            (if (>= (count c) 9)
                              (-> c (nth 8) expr)
                              nil))
      "["                   (-> c (nth 2) expr)
      :annotated-expression (-> c first expr))))

(defmethod expr :annotated-expression [{:keys [c]}]
  (if (> (count c) 1)
    (->Annot
     (expr (nth c 0))
     (expr (nth c 4)))
    (expr (first c))))

(defmethod expr :empty-collection [{:keys [c t]}]
  (let [typ       (-> c last expr)]
    (if (= :List (-> c (nth 4) :t))
      (->ListLit     typ [])
      (->OptionalLit typ nil))))

(defmethod expr :non-empty-optional [{:keys [c t]}]
  (let [typ   (-> c last  expr)
        value (-> c first expr)]
    (->OptionalLit typ value)))

(defn identifier [e]
  (let [children (:c e)
        ;; if we have  a simple identifier, the "prefix" is just the label
        var (->> children first expr)
        ;; at the end of `children` there might be a DeBrujin index
        maybe-index (-> children last)
        index? (= :natural-literal (:t maybe-index))
        index (if index?
                (-> maybe-index :c first :c first read-string)
                0)]
    (case var
      "Bool"              (->BoolT)
      "Optional"          (->OptionalT)
      "None"              (->None)
      "Natural"           (->NaturalT)
      "Integer"           (->IntegerT)
      "Double"            (->DoubleT)
      "Text"              (->TextT)
      "List"              (->ListT)
      "True"              (->BoolLit true)
      "False"             (->BoolLit false)
      "Type"              (->Const :type)
      "Kind"              (->Const :kind)
      "Sort"              (->Const :sort)
      "Natural/fold"      (->NaturalFold)
      "Natural/build"     (->NaturalBuild)
      "Natural/isZero"    (->NaturalIsZero)
      "Natural/even"      (->NaturalEven)
      "Natural/odd"       (->NaturalOdd)
      "Natural/toInteger" (->NaturalToInteger)
      "Natural/show"      (->NaturalShow)
      "Integer/toDouble"  (->IntegerToDouble)
      "Integer/show"      (->IntegerShow)
      "Double/show"       (->DoubleShow)
      "List/build"        (->ListBuild)
      "List/fold"         (->ListFold)
      "List/length"       (->ListLength)
      "List/head"         (->ListHead)
      "List/last"         (->ListLast)
      "List/indexed"      (->ListIndexed)
      "List/reverse"      (->ListReverse)
      "Optional/fold"     (->OptionalFold)
      "Optional/build"    (->OptionalBuild)
      "Text/show"         (->TextShow)
      (->Var var index))))

(defmethod expr :nonreserved-label [{:keys [c]}]
  (let [quoted? (-> c first string?) ;; a quoted label is preceded by `
        c (if quoted?
            (-> c rest butlast)
            c)]
    (compact c)))

(defmethod expr :identifier [e]
  (identifier e))

(defmacro defexpr*
  "Generalize `defmethod` for the cases in which we need to do
  something like:
  - if there's one remove this tag
  - if there's multiple create an `Expr a b` and recur with left-precedence"
  [parser-tag record-class separator]
  (let [expr-constructor (symbol (str "->" record-class))]
    `(defmethod expr ~parser-tag [e#]
       (if (> (count (:c e#)) 1)
         (let [exprs# (remove #(or (= ~separator (if (string? %) % (:t %)))
                                   (= :whsp1 (:t %))
                                   (= :whsp (:t %)))
                              (:c e#))]
           (loop [more# (nnext exprs#)
                  start# (~expr-constructor
                           (expr (first exprs#))
                           (expr (second exprs#)))]
             (if (empty? more#)
               start#
               (recur (rest more#)
                      (~expr-constructor start# (expr (first more#)))))))
         (expr (-> e# :c first))))))


(defexpr* :import-alt-expression    ImportAlt    "?")
(defexpr* :or-expression            BoolOr       "||")
(defexpr* :plus-expression          NaturalPlus  "+")
(defexpr* :text-append-expression   TextAppend   "++")
(defexpr* :list-append-expression   ListAppend   "#")
(defexpr* :and-expression           BoolAnd      "&&")
(defexpr* :combine-expression       Combine      :combine)
(defexpr* :prefer-expression        Prefer       :prefer)
(defexpr* :combine-types-expression CombineTypes :combine-types)
(defexpr* :times-expression         NaturalTimes "*")
(defexpr* :equal-expression         BoolEQ       "==")
(defexpr* :not-equal-expression     BoolNE       "!=")

(defmethod expr :application-expression [{:keys [c t]}]
  (if (> (count c) 1)
    (let [exprs (remove #(= :whsp1 (:t %)) c)
          some? (= :Some (-> c first :t))]
      (loop [more (nnext exprs)
             app (cond
                   some?
                   (->Some (expr (second exprs)))

                   :else
                   (->App
                     (expr (first exprs))
                     (expr (second exprs))))]
        (if (empty? more)
          app
          (recur (rest more)
                 (->App app (expr (first more)))))))
    (expr (-> c first))))

(defmethod expr :selector-expression [e]
  (if (children? e)
    (let [exprs (remove #(or (= :whsp (:t %)) (= "." %)) (:c e))
          base  (expr (first exprs))
          labels? (fn [l] (= :labels (:t l)))]
      (loop [more (nnext exprs)
             sel ((if (labels? (-> exprs second :c first))
                    ->Project
                    ->Field)
                  base
                  (expr (-> exprs second :c first)))]
        (if (empty? more)
          sel
          (recur (rest more)
                 ((if (labels? (-> more first :c first))
                    ->Project
                    ->Field)
                  sel
                  (expr (-> more first :c first)))))))
    (-> e :c first expr))) ;; Otherwise we go to the primitive expression

(defmethod expr :labels [{:keys [c t]}]
  (->> c
     (remove #(or (string? %)
                  (= :whsp (:t %))))
     (mapv expr)))

(defmethod expr :double-literal [e]
  (let [first-tag (-> e :c first :t)
        children (:c e)]
    (case first-tag
      :numeric-double-literal (let [d (-> children first compact read-string)]
                                (if (or (= d Double/NEGATIVE_INFINITY)
                                        (= d Double/POSITIVE_INFINITY))
                                  (fail/double-out-of-bounds! (-> children first compact) e)
                                  (->DoubleLit d)))
      :NaN                    (->DoubleLit Double/NaN)
      :minus-infinity-literal (->DoubleLit Double/NEGATIVE_INFINITY)
      :plus-infinity-literal  (->DoubleLit Double/POSITIVE_INFINITY))))

(defmethod expr :primitive-expression [e]
  (let [first-tag (or (-> e :c first :t) (-> e :c first))
        children (:c e)]
    (case first-tag
      :double-literal         (-> children first expr)
      :natural-literal        (-> children first compact read-string ->NaturalLit)
      :integer-literal        (-> children first compact read-string ->IntegerLit)
      :text-literal           (-> children first expr)
      "{"                     (-> children (nth 2) expr)
      "<"                     (-> children (nth 2) expr)
      :non-empty-list-literal (-> children first expr)
      :reserved-clj           (-> children first expr)
      :identifier             (-> children first expr)
      "("                     (-> children second expr))))

(defmethod expr :union-type-or-literal [e]
  (let [first-tag (-> e :c first :t)]
    (case first-tag
      :non-empty-union-type-or-literal
      (let [[k v kvs] (->> e :c first expr)]
        (if (and k v) ;; if we actually have a value, we have a Union Literal
          (->UnionLit k v kvs)
          (->UnionT kvs)))
      (->UnionT {})))) ;; Empty union type

(defmethod expr :union-literal-variant-value [{:keys [c]}]
  [(expr (nth c 2))
   (->> c
      (remove
       #(or (string? %)
            (= :whsp (:t %))
            (= :whsp1 (:t %))))
      rest
      (mapv expr)
      (apply merge {}))])

(defmethod expr :union-type-entry [{:keys [c]}]
  {(expr (nth c 0))
   (when (> (count c) 1)
     (expr (nth c 4)))})

;; This should return a vector `[k v kvs]`,
;; where `k` is the value key, `v` is its value,
;; and `kvs` are the remaining types.
;; `k` and `v` can be `nil`, and that means it's a Union type
(defmethod expr :non-empty-union-type-or-literal [{:keys [c] :as e}]
  (let [label (-> c first expr)]
    (if (> (count c) 1)
      (let [tag (-> (nth c 2) :t)
            [val? typ? kvs] (expr (nth c 2))]
        (if (= tag :union-literal-variant-value)
          (let [[val kvs] (expr (nth c 2))]
            [label val kvs])
          (let [[typ? [valk? valv? kvs]] (expr (nth c 2))]
            [valk? valv? (merge {label typ?} kvs)])))
      [nil nil {label nil}])))


(defmethod expr :union-type-or-literal-variant-type [{:keys [c]}]
  [(when (= ":" (first c))
     (expr (nth c 2)))
   (if (> (count c) 3)
     (expr (last c))
     [nil nil {}])])

(defmethod expr :record-type-or-literal [e]
  (let [first-tag (-> e :c first :t)]
    (case first-tag
      :empty-record-literal             (->RecordLit {})
      :non-empty-record-type-or-literal (-> e :c first expr)
      (->RecordT {}))))

(defmethod expr :record-type-entry [{:keys [c]}]
  {(expr (nth c 0)) (expr (nth c 4))})

(defmethod expr :record-literal-entry [{:keys [c]}]
  {(expr (nth c 0)) (expr (nth c 4))})

(defmethod expr :non-empty-record-type-or-literal [e]
  (let [first-label (-> e :c first expr)
        other-vals (-> e :c (nth 2))
        record-literal? (= (:t other-vals) :non-empty-record-literal)
        other-vals (remove
                    #(or (string? %)
                         (= :whsp (:t %))
                         (= :whsp1 (:t %)))
                    (:c other-vals))
        [first-val other-kvs] [(-> other-vals first expr)
                               (->> other-vals
                                  rest
                                  (mapv expr)
                                  (apply merge))]]
    ((if record-literal?
       ->RecordLit
       ->RecordT)
     (merge {first-label first-val} other-kvs))))

(defmethod expr :label [e]
  (let [quoted? (-> e :c first string?) ;; a quoted label is preceded by `
        actual-label ((if quoted? second first) (:c e))
        str-label (->> actual-label :c compact)]
     str-label))

(defmethod expr :non-empty-list-literal [e]
  ;; Here we always pass the type as nil, because it's a non empty list,
  ;; and the eventual type annotation has this wrapped in an Annot
  (let [vals (->> e :c (remove #(or (string? %) (= :whsp (:t %)))) (mapv expr))]
    (->ListLit nil vals)))


(defn multiline->text-lit [lines]
  (let [min-indent (apply
                     min
                     (mapv
                       (fn [[first-el & others]]
                         (if (string? first-el)
                           (count (take-while #(= % \space) first-el))
                           0))
                       lines))
        strip-indent (fn [line]
                       (if (and (> min-indent 0) (string? (first line)))
                         (update line 0 #(subs % min-indent))
                         line))]
    (->> lines
       (mapcat strip-indent)
       (compact-chunks)
       (into []))))


;; From https://groups.google.com/forum/#!topic/clojure/JrnYQp84Dig
(defn hex->num [#^String s]
  (Integer/parseInt (.substring s 2) 16))


(defmethod expr :text-literal [e]
  (let [first-tag (-> e :c first :t)
        children (:c e)]
    (->TextLit
      (if (= first-tag :double-quote-literal)
        ;; If it's a double quoted string, we fold on the children,
        ;; so that we collapse the contiguous strings in a single chunk,
        ;; while skipping the interpolation expressions
        (loop [children (-> children first :c rest butlast) ;; Skip the quotes
               acc ""
               chunks []]
          (if (seq children)
            (let [chunk (first children)
                  content (:c chunk)]
              (if (string? (first content))
                ;; If it's a string we're matching the escape slash '\',
                ;; so we emit some special chars
                (let [new-char (condp = (-> content second :c first)
                                 "\"" "\""
                                 "$"  "$"
                                 "\\" "\\"
                                 "/"  "/"
                                 "b"  "\b"
                                 "f"  "\f"
                                 "n"  "\n"
                                 "r"  "\r"
                                 "t"  "\t"
                                 ;; Otherwise we're reading in a \uXXXX char
                                 (->> (-> content second :c rest)
                                    compact
                                    (str "0x")
                                    hex->num
                                    char))]
                  (recur (rest children)
                         (str acc new-char)
                         chunks))
                ;; Otherwise we match on the tag of the rule
                (condp = (-> content first :t)
                  ;; If we match the interpolation, empty the accumulator and concat it
                  :interpolation
                  (recur (rest children)
                         ""
                         (conj chunks acc (expr (-> content first :c second))))
                  ;; Otherwise we just attach to the accumulator, since it's a normal char
                  :double-quote-char
                  (recur (rest children)
                         (str acc (-> content first :c first))
                         chunks))))
            ;; If we have no children left to process,
            ;; we return the chunks we have, plus the accomulator
            (conj chunks acc)))
        ;; Otherwise it's a single quote literal,
        ;; so we recur over the children until we find an ending literal.
        ;; As above, we make expressions out of interpolation syntax
        (loop [children (-> children first :c (nth 2) :c)
               acc ""
               line-chunks []
               lines []]
          (if (= children ["''"])
            (multiline->text-lit (conj lines (conj line-chunks acc)))
            (condp = (-> children first :t)
              :interpolation ;; Interpolation - reset accumulator, concat to chunks
              (recur (-> children second :c)
                     ""
                     (conj line-chunks acc (expr (-> children first :c second)))
                     lines)

              :escaped-quote-pair ;; Escaping the single quotes
              (recur (-> children second :c)
                     (str acc "''")
                     line-chunks
                     lines)

              :escaped-interpolation ;; Escaping the interpolation symbols
              (recur (-> children second :c)
                     (str acc "${")
                     line-chunks
                     lines)

              :single-quote-char
              (condp = (-> children first :c first)
                {:c '("\n") :t :end-of-line} ;; newline, reset acc and line-chunks
                (recur (-> children second :c)
                       ""
                       []
                       (conj lines (conj line-chunks (str acc "\n"))))

                {:c '("\t") :t :tab} ;; tab char, we leave it alone
                (recur (-> children second :c)
                       (str acc "\t")
                       line-chunks
                       lines)

                ;; Otherwise we just add the string to the accumulator and recur
                (recur (-> children second :c)
                       (str acc (-> children first :c first))
                       line-chunks
                       lines)))))))))

;; Default case, we end up here when there is no matches
(defmethod expr :default [e]
  (fail/ast-building! e))
