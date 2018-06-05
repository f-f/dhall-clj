(ns dhall-clojure.in.parse
  (:require [instaparse.core :as insta]
            [clojure.java.io :as io]
            [dhall-clojure.in.core :refer :all]))

(def grammar (slurp (io/resource "dhall.abnf")))

(def dhall-parser
  (insta/parser grammar
                :input-format :abnf
                :start :complete-expression
                :output-format :enlive))

(defn clean
  "Cut the names of the attrs of the tree
  TODO: save the meta?"
  [tree]
  (if (map? tree)
    {:c (mapv clean (:content tree))
     ;;:a (:attrs tree)
     :t (:tag tree)}
    tree))

(def parse (comp clean dhall-parser))

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

;;
;; Useful rules start here
;;
(defmethod expr :expression [e]
  (let [first-tag (-> e :c first :t)
        children (:c e)]
    (case first-tag
      :lambda ""
      :if (->BoolIf
            (expr (nth children 1))
            (expr (nth children 3))
            (expr (nth children 5)))
      :let ""
      :forall ""
      :operator-expression ""
      :annotated-expression (expr (first children)))))

(defmethod expr :annotated-expression [e]
  (let [first-tag (-> e :c first :t)
        children (:c e)]
    (case first-tag
      :merge ""
      :open-bracket ""
      :operator-expression (if (> (count children) 1)
                             (->Annot
                               (expr (first children))
                               (expr (nth children 2)))
                             (expr (first children))))))


(defmethod expr :or-expression [e]
  (if (> (count (:c e)) 1)
    (let [exprs (remove #(= :or (:t %)) (:c e))]
      (loop [more (nnext exprs)
             or (->BoolOr
                  (expr (first exprs))
                  (expr (second exprs)))]
        (if (empty? more)
          or
          (recur (rest more)
                 (->BoolOr or (expr (first more)))))))
    (expr (-> e :c first))))

(defmacro defexpr*
  "Generalize `defmethod` for the cases in which we need to do
  something like:
  - if there's one remove this tag
  - if there's multiple create an `Expr a b` and recur with left-precedence"
  [parser-tag record-class separator-tag]
  (let [expr-constructor (symbol (str "->" record-class))]
    `(defmethod expr ~parser-tag [e#]
       (if (> (count (:c e#)) 1)
         (let [exprs# (remove #(= ~separator-tag (:t %)) (:c e#))]
           (loop [more# (nnext exprs#)
                  start# (~expr-constructor
                           (expr (first exprs#))
                           (expr (second exprs#)))]
             (if (empty? more#)
               start#
               (recur (rest more#)
                      (~expr-constructor start# (expr (first more#)))))))
         (expr (-> e# :c first))))))

(defexpr* :plus-expression          NaturalPlus  :plus)
(defexpr* :text-append-expression   TextAppend   :text-append)
(defexpr* :list-append-expression   ListAppend   :list-append)
(defexpr* :and-expression           BoolAnd      :and)
(defexpr* :combine-expression       Combine      :combine)
(defexpr* :prefer-expression        Prefer       :prefer)
(defexpr* :combine-types-expression CombineTypes :combine-types)
(defexpr* :times-expression         NaturalTimes :times)
(defexpr* :equal-expression         BoolEQ       :double-equal)
(defexpr* :not-equal-expression     BoolNE       :not-equal)

;; TODO: support `constructors`
(defexpr* :application-expression App :whitespace-chunk)

(defmethod expr :import [e]
  e) ;; TODO

(defmethod expr :selector-expression [e]
  (if (children? e)
    "TODO handle accessor fields"
    (first-child-expr e))) ;; Otherwise we go to the primitive expression

(defmethod expr :primitive-expression [e]
  (let [first-tag (-> e :c first :t)
        children (:c e)]
    (case first-tag
      :double-literal (-> children first compact read-string ->DoubleLit)
      :natural-literal (-> children first compact read-string ->NaturalLit)
      :integer-literal (-> children first compact read-string ->IntegerLit)
      :text-literal (-> children first expr)
      :open-brace ""
      :open-angle ""
      :non-empty-list-literal ""
      :identifier-reserved-namespaced-prefix ""
      :reserved-namespaced ""
      :identifier-reserved-prefix ""
      :reserved ""
      :identifier ""
      :open-parens "")))

(defmethod expr :text-literal [e]
  (let [first-tag (-> e :c first :t)
        children (:c e)]
    (->TextLit
      (if (= first-tag :double-quote-literal)
        ;; If it's a double quoted string, we fold on the children,
        ;; so that we collapse the contiguous strings in a single chunk,
        ;; while skipping the interpolation expressions
        (loop [children (-> children first :c rest butlast) ;; Skip the quotes
               acc nil
               chunks []]
          (if (seq children)
            (let [chunk (first children)
                  content (:c chunk)]
              (if (every? string? content)  ;; If they are not strings, it's an interpolation
                (recur (rest children)
                       (str acc (apply str content))
                       chunks)
                (recur (rest children)
                       nil
                       (conj chunks acc (expr (nth content 1))))))
            ;; If we have no children left to process,
            ;; we return the chunks we have, plus the accomulator
            (if-not acc
              chunks
              (conj chunks acc))))
        ;; Otherwise it's a single quote literal,
        ;; so we recur over the children until we find an ending literal.
        ;; As above, we make expressions out of interpolation syntax
        (loop [children (-> children first :c second :c)
               acc nil
               chunks []]
          (if (= children ["''"])
            (if-not acc  ;; If we have chars left in acc
              chunks
              (conj chunks acc))
            (if (not= (first children) "${")  ;; Check if interpolation
              ;; If not we just add the string and recur
              (recur (-> children second :c)
                     (str acc (first children))
                     chunks)
              (recur (-> children (nth 3) :c)
                     nil
                     (conj chunks acc (expr (second children)))))))))))

;; Default case, we end up here when there is no matches
(defmethod expr :default [e]
  (println "Hitting default case")
  (println e)
  e)

