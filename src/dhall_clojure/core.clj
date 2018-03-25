(ns dhall-clojure.core
  (:require [instaparse.core :as insta]
            [clojure.string :as string]
            [clojure.walk :refer [postwalk prewalk walk]]))

;;;;;; UTIL
;; Credit: https://gist.github.com/danielpcox/c70a8aa2c36766200a95#gistcomment-2313926
;; TODO: check if it makes sense
(defn deep-merge [v & vs]
  (letfn [(rec-merge [v1 v2]
            (if (and (map? v1) (map? v2))
              (merge-with deep-merge v1 v2)
              v2))]
    (if (some identity vs)
      (reduce #(rec-merge %1 %2) v vs)
      v)))



(defn emit-record-type-or-literal [& child]
  ;; Check if it's not an empty map
  ;; TODO actually differentiate the Type case
  (if (and (seq []) (= (first child) :non-empty-record-type-or-literal))
    (first child)
    {}))



;; TODO: move the grammar to resources?
(def grammar (slurp "dhall-lang/standard/dhall.abnf"))

(def dhall-parser
  (insta/parser grammar
                :input-format :abnf
                :start :complete-expression
                :output-format :hiccup))

(declare emit-expression)

(defn emit-lambda [open-parens param colon param-type close-parens arrow expression]
  ;; TODO: type check on param-type
  ;; TODO: emit symbol for param, it's a label
  `(fn [] ~(apply emit-expression expression)))

(defn make-double [digits]
  ;; TODO handle exponent
  (->> digits
     (map (fn [tag]
            (if (vector? tag)
              (second tag)
              tag)))
     (apply str)
     (read-string)))

(defn make-num [digits]
  (->> digits
     (map (fn [tag]
            (if (vector? tag)
              (second (second tag))
              tag)))
     (apply str)
     (read-string)))

(defn emit-primitive-expression [[tag [first-tag & others] & content]]
  (case first-tag
    :double-literal (make-double others)
    :integer-literal (make-num others)
    :natural-literal (make-num others)
    ;:text-literal
    ;:open-brace
    ;:open-angle
    ;:non-empty-list-literal
    ;:import
    ;:identifier
    ;:Natural-fold
    ;:Natural-build
    ;:Natural-isZero
    ;:Natural-even
    ;:Natural-odd
    ;:Natural-toInteger
    ;:Natural-show
    :True true
    :False false
    [first-tag others])) ;; Here for debug

(defn emit-selector-expression
  "Accessing map fields"
  [[tag primitive-expr & selectors]]
  ;; TODO handle the actual dot accessor
  ;(emit-primitive-expression primitive-expr))
  [primitive-expr selectors])

(defn emit-application-expression
  "Function application, it's just a list"
  [[tag & content]]
  ;; TODO: support constructors
  ;; TODO FIXME: handle the record accessors being split in different selectors
  ;(println content)
  (if (seq (rest content))
    (map emit-selector-expression content)
    (emit-selector-expression (first content))))

(defmacro defemit*
  [this-emit key sym next-emit]
  `(defn ~this-emit [[tag# & exprs#]]
     ;(println (str ~this-emit))
     (if-not (seq (rest exprs#))
       (do
         ;(println exprs#)
         (~next-emit (first exprs#)))
       (let [tag-free# (remove #(= ~key (first %)) exprs#)]
         ;(println (str "Multiple: " (into [] tag-free#)))
         (list* ~sym (mapv ~next-emit tag-free#))))))

(defemit* emit-not-equal-expression   :not-equal   'not=       emit-application-expression)
(defemit* emit-equal-expression       :equal       '=          emit-not-equal-expression)
(defemit* emit-times-expression       :times       '*          emit-equal-expression)
(defemit* emit-prefer-expression      :prefer      'merge      emit-times-expression)
(defemit* emit-combine-expression     :combine     'deep-merge emit-prefer-expression)
(defemit* emit-and-expression         :and         'and        emit-combine-expression)
(defemit* emit-list-append-expression :list-append 'concat     emit-and-expression)
(defemit* emit-text-append-expression :text-append 'str        emit-list-append-expression)
(defemit* emit-plus-expression        :plus        '+          emit-text-append-expression)
(defemit* emit-or-expression          :or          'or         emit-plus-expression)



(defn emit-expression [[first-tag & inner-content] & content]
  ;;(println (str "first: " first-tag))
  ;;(println inner-content)
  (case first-tag
    :lambda               (apply emit-lambda content)
    :if                   "TODO"
    :let                  "TODO"
    :forall               "TODO"
    :operator-expression  (do
                            ;(println content) ;; TODO: wasn't this supposed to contain an arrow as well?
                            (apply emit-or-expression inner-content))
    :annotated-expression (apply emit-expression inner-content)))


(defn emit-complete-expression
  "Just discard whitespace here"
  [whitespace expression]
  expression)


;;;;; API

(defn input
  "Given a spec and a Dhall expression, parse the expression
  and return a Clojure form conformed to the spec.
  When no spec is given, still runs the Dhall typechecker,
  but doesn't conform to a spec."
  ;; TODO: Handle this case
  ([expr] (input nil expr))
  ([spec expr]
   (let [transform-map {:complete-expression    emit-complete-expression
                        :expression             emit-expression
                        :record-type-or-literal emit-record-type-or-literal}
         ;; TODO: use insta/failure? to check if the parsing was fine, fail early if not
         ;; We should probably use the error monad here.
         parsed (dhall-parser expr)]           ;; Parse: Dhall  -> Hiccup
     ;; TODO: typecheck
     ;; TODO: normalize
     (insta/transform transform-map parsed)))) ;; Emit:  Hiccup -> Clojure
