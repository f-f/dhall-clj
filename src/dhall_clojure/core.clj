(ns dhall-clojure.core
  (:require [instaparse.core :as insta]
            [clojure.string :as string]
            [clojure.core.match :refer [match]]
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


;; TODO: move the grammar to resources?
(def grammar (slurp "dhall-lang/standard/dhall.abnf"))

(def dhall-parser
  (insta/parser grammar
                :input-format :abnf
                :start :complete-expression
                :output-format :hiccup))




(defn emit-record-type-or-literal [& child]
  ;; Check if it's not an empty map
  ;; TODO actually differentiate the Type case
  (if (and (seq []) (= (first child) :non-empty-record-type-or-literal))
    (first child)
    {}))



(defn emit-double [digits]
  ;; TODO handle exponent
  (->> digits
     (map (fn [tag]
            (if (vector? tag)
              (second tag)
              tag)))
     (apply str)
     (read-string)))

(defn emit-num [[tag & digits] & {:keys [negative]}]
  (->> digits
     (map second)
     (apply str)
     (read-string)
     (#(if negative (- %) %))))

(defn emit-string [[[tag & chars] whitespace]]
  (->> chars
     (rest)        ;; The first and
     (butlast)     ;;  the last char are quotes, so we ignore them
     (mapv second) ;; Every char is a two-element vector, take the actual char
     (apply str))) ;; Join

(defn emit-list [content]
  ;; Take only elements with even index, as the odd ones are parens and commas
  (take-nth 2 (rest content)))

(defn emit-primitive-expression [& content]
  (match (into [] content)
    [[:double-literal & digits]]        (emit-double digits)
    [[:natural-literal plus  digits _]] (emit-num digits)
    [[:integer-literal minus digits _]] (emit-num digits :negative true)
    [[:integer-literal       digits _]] (emit-num digits)
    [[:text-literal & characters]]      (emit-string characters)
    [[:non-empty-list-literal & elems]] (emit-list elems)
    :else (do (println "Failed match: primitive-expression")
              content)))

(defn emit-selector-expression
  "Accessing map fields"
  [[tag primitive-expr & selectors]]
  ;; TODO handle the actual dot accessor
  (emit-primitive-expression primitive-expr))

(defn emit-application-expression
  "Function application, it's just a list"
  [[& content]]
  ;; TODO: support constructors
  (if (seq (rest content))
    (map #(emit-selector-expression (second %)) content)
    (emit-selector-expression (-> content first second))))

(defmacro defemit*
  [this-emit key sym next-emit]
  `(defn ~this-emit [[& exprs#]]
     ;;(println (str ~this-emit))
     ;;(println exprs#)
     ;; Check that we're actually matching on the correct key
     ;; The tag of the first element is the next expression we're evaluating
     (assert (= (-> exprs# ffirst) ~(keyword (subs (str next-emit) 5))))
     (let [unwrap# #(~next-emit (if (seq (rest %))
                                  (rest %)
                                  [(second %)]))]
       (if-not (seq (rest exprs#))
         (do
           ;;(println (str "SINGLE: " exprs#))
           (unwrap# (first exprs#)))
         (let [tag-free# (remove #(= ~key (first %)) exprs#)]
           ;;(println (str "MULTIPLE: " (into [] tag-free#)))
           (list* ~sym (mapv unwrap# tag-free#)))))))

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

(defn emit-annotated-expression [& content]
  (match (into [] content)
    [[:merge]]                           content ;; TODO
    [[:open-bracket]]                    content ;; TODO
    [[:operator-expression inner] _ typ] inner   ;; TODO add typ as meta
    [[:operator-expression
      [:or-expression & inner]]]          (emit-or-expression inner)
    :else (do (println "Failed match: annotated-expression")
              content)))

;;;;; Transform starting points

(defn emit-expression [& content]
  (match (into [] content)
    [[:lambda _ _] _ arg _ typ _ _ body] `(fn [~arg] ~body) ;; TODO add typ as meta
    [[:if _ _] t _then e1 _else e2]      `(if ~t ~e1 ~e2)
    ;; TODO let typed
    ;; TODO let untyped
    ;; TODO forall
    ;[[:operator-expression inner] _ e1]  e1 ;; TODO what does this form mean?
    [[:annotated-expression inner]]      (emit-annotated-expression inner)
    :else (do (println "Failed match: expression")
              content)))
  
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
   ;; We have only some "starter" rules here in the transform-map, because
   ;; in order to match the rules contained in other rules we need to go
   ;; top-down. So we use these to "kickstart" the emission, and descend
   ;; to the leaves of the tree with them.
   (let [transform-map {:complete-expression    emit-complete-expression
                        :expression             emit-expression}
         ;; TODO: use insta/failure? to check if the parsing was fine, fail early if not
         ;; NOTE: we should probably use the error monad here.
         parsed (dhall-parser expr)]           ;; Parse: Dhall  -> Hiccup
     ;; TODO: typecheck
     ;; TODO: normalize
     (insta/transform transform-map parsed)))) ;; Emit:  Hiccup -> Clojure
