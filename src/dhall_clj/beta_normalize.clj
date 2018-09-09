(ns dhall-clj.beta-normalize
  (:require [dhall-clj.alpha-normalize :refer [alpha-normalize]]
            [dhall-clj.ast :refer :all]
            [medley.core :refer [map-vals]])
  (:import [dhall_clj.ast NaturalLit TextLit BoolLit Lam App ListBuild ListFold
            NaturalBuild NaturalFold OptionalBuild OptionalFold NaturalIsZero
            NaturalEven NaturalOdd NaturalToInteger NaturalShow IntegerLit
            IntegerShow IntegerToDouble DoubleLit DoubleShow ListLit ListLength
            ListHead ListLast ListIndexed ListReverse RecordLit
            UnionLit RecordT UnionT Some None]))

(defprotocol IBetaNormalize
  (beta-normalize [this]
    "Reduce an expression to its normal form, performing beta reduction"))


(defn judgmentally-equal
  "Returns `true` if two expressions are α-equivalent and β-equivalent and
  `false` otherwise"
  [a b]
  (let [ab-normalize (comp beta-normalize alpha-normalize)]
    (= (ab-normalize a)
       (ab-normalize b))))


(extend-protocol IBetaNormalize

  dhall_clj.ast.Const
  (beta-normalize [this] this)

  dhall_clj.ast.Var
  (beta-normalize [this] this)

  dhall_clj.ast.Lam
  (beta-normalize [this]
    (-> this
       (update :type beta-normalize)
       (update :body beta-normalize)))

  dhall_clj.ast.Pi
  (beta-normalize [this]
    (-> this
       (update :type beta-normalize)
       (update :body beta-normalize)))

  dhall_clj.ast.App
  (beta-normalize [this]
    (let [f  (:a this)
          a  (:b this)
          f' (beta-normalize f)]
      (if (instance? Lam f')
        (let [{:keys [arg type body]} f'
              v  (->Var arg 0)
              a' (shift a 1 v)]
          (-> body
             (subst v a')
             (shift -1 v)
             beta-normalize))
        (let [a' (beta-normalize a)]
          (cond
            ;; build/fold fusion for List
            (and (instance? App f')
                 (instance? ListBuild (:a f'))
                 (instance? App a')
                 (instance? App (:a a'))
                 (instance? ListFold (:a (:a a'))))
            (beta-normalize (:b a'))

            ;; build/fold fusion for Natural
            (and (instance? NaturalBuild f')
                 (instance? App a')
                 (instance? NaturalFold (:a a')))
            (beta-normalize (:b a'))

            ;; build/fold fusion for Optional
            (and (instance? App f')
                 (instance? OptionalBuild (:a f'))
                 (instance? App a')
                 (instance? App (:a a'))
                 (instance? OptionalFold (:a (:a a'))))
            (beta-normalize (:b a'))

            ;; NaturalFold
            (and (instance? App         f')
                 (instance? App         (:a f'))
                 (instance? App         (:a (:a f')))
                 (instance? NaturalFold (:a (:a (:a f'))))
                 (instance? NaturalLit  (:b (:a (:a f')))))
            (let [zero  a'
                  succ' (:b f')
                  t     (:b (:a f'))
                  n0    (:n (:b (:a (:a f'))))]
              (letfn [(go [n]
                        (if (zero? n)
                          (beta-normalize zero)
                          (beta-normalize (->App succ' (go (dec n))))))]
                (go n0)))

            ;; NaturalBuild
            (instance? NaturalBuild f')
            (let [zero (->NaturalLit 0)
                  succ (->Lam "x"
                              (->NaturalT)
                              (->NaturalPlus (->Var "x" 0) (->NaturalLit 1)))]
              (beta-normalize (->App (->App (->App a' (->NaturalT)) succ) zero)))

            ;; NaturalIsZero
            (and (instance? NaturalLit a')
                 (instance? NaturalIsZero f'))
            (->BoolLit (= 0 (:n a')))

            ;; NaturalEven
            (and (instance? NaturalLit a')
                 (instance? NaturalEven f'))
            (->BoolLit (even? (:n a')))

            ;; NaturalOdd
            (and (instance? NaturalLit a')
                 (instance? NaturalOdd f'))
            (->BoolLit (odd? (:n a')))

            ;; NaturalToInteger
            (and (instance? NaturalLit a')
                 (instance? NaturalToInteger f'))
            (->IntegerLit (:n a'))

            ;; NaturalShow
            (and (instance? NaturalLit a')
                 (instance? NaturalShow f'))
            (->TextLit [(str (:n a'))])

            ;; IntegerShow
            (and (instance? IntegerLit a')
                 (instance? IntegerShow f'))
            (->TextLit [(let [n (:n a')]
                          (str (when (>= n 0) "+") n))])

            ;; IntegerToDouble
            (and (instance? IntegerLit a')
                 (instance? IntegerToDouble f'))
            (->DoubleLit (double (:n a')))

            ;; DoubleShow
            (and (instance? DoubleLit a')
                 (instance? DoubleShow f'))
            (->TextLit [(str (:n a'))])

            ;; OptionalBuild
            (and (instance? App f')
                 (instance? OptionalBuild (:a f')))
            (let [typ  (:b f')
                  typ' (shift typ 1 (->Var "a" 0))
                  optional (->App (->OptionalT) typ)
                  just     (->Lam "a"
                                  typ
                                  (->Some (->Var "a" 0)))
                  nothing  (->App (->None) typ)
                  res (->App (->App (->App a' optional)
                                   just)
                             nothing)]
              (beta-normalize res))

            ;; ListBuild
            (and (instance? App f')
                 (instance? ListBuild (:a f')))
            (let [typ  (:b f')
                  typ' (shift typ 1 (->Var "a" 0))
                  _list (->App (->ListT) typ)
                  _cons (->Lam
                          "a"
                          typ
                          (->Lam
                            "as"
                            (->App (->ListT) typ')
                            (->ListAppend
                              (->ListLit nil [(->Var "a" 0)])
                              (->Var "as" 0))))
                  _nil  (->ListLit typ [])]
              (beta-normalize
                (->App (->App (->App a' _list)
                              _cons)
                       _nil)))

            ;; ListFold
            (and (instance? App      f')
                 (instance? App      (:a f'))
                 (instance? App      (:a (:a f')))
                 (instance? App      (:a (:a (:a f'))))
                 (instance? ListFold (:a (:a (:a (:a f')))))
                 (instance? ListLit  (:b (:a (:a f')))))
            (let [_nil  a'
                  _cons (:b f')
                  t     (:b (:a f'))
                  xs    (:exprs (:b (:a (:a f'))))
                  fold  (fn [y ys]
                          (beta-normalize (->App (->App _cons ys) y)))]
              (reduce fold (beta-normalize _nil) (reverse xs)))

            ;; ListLength
            (and (instance? App f')
                 (instance? ListLength (:a f'))
                 (instance? ListLit a'))
            (->NaturalLit (count (:exprs a')))

            ;; ListHead
            (and (instance? App f')
                 (instance? ListHead (:a f'))
                 (instance? ListLit a'))
            (beta-normalize
              (if-let [el (first (:exprs a'))]
                (->Some el)
                (->App (->None) (:b f'))))

            ;; ListLast
            (and (instance? App f')
                 (instance? ListLast (:a f'))
                 (instance? ListLit a'))
            (beta-normalize
              (if-let [el (last (:exprs a'))]
                (->Some el)
                (->App (->None) (:b f'))))

            ;; ListIndexed
            (and (instance? App f')
                 (instance? ListIndexed (:a f'))
                 (instance? ListLit a'))
            (let [t2 (->RecordT {"index" (->NaturalT)
                                 "value" (:b f')})
                  xs (:exprs a')
                  typ? (when (empty? xs)
                         t2)
                  adapt (fn [i el]
                          (->RecordLit
                            {"index" (->NaturalLit i)
                             "value" el}))
                  xs' (map-indexed adapt xs)]
              (beta-normalize (->ListLit typ? xs')))

            ;; ListReverse
            (and (instance? App f')
                 (instance? ListReverse (:a f'))
                 (instance? ListLit a'))
            (let [xs   (:exprs a')
                  typ? (when (empty? xs)
                         (:b f'))]
              (beta-normalize (->ListLit typ? (reverse xs))))

            ;; OptionalFold - None
            (and (instance? App          f')
                 (instance? App          (:a f'))
                 (instance? App          (:a (:a f')))
                 (instance? App          (:a (:a (:a f'))))
                 (instance? OptionalFold (:a (:a (:a (:a f')))))
                 (instance? App          (:b (:a (:a f'))))
                 (instance? None         (:a (:b (:a (:a f'))))))
            (beta-normalize a')

            ;; OptionalFold - Some
            (and (instance? App          f')
                 (instance? App          (:a f'))
                 (instance? App          (:a (:a f')))
                 (instance? App          (:a (:a (:a f'))))
                 (instance? OptionalFold (:a (:a (:a (:a f')))))
                 (instance? Some         (:b (:a (:a f')))))
            (let [just     (:b f')
                  val      (:e (:b (:a (:a f'))))]
              (beta-normalize (->App just val)))

            :else (->App f' a'))))))

  dhall_clj.ast.Let
  (beta-normalize [{:keys [label body next]}]
    (let [var   (->Var label 0)
          body' (shift body 1 var)]
      (beta-normalize
        (-> next
           (subst var body')
           (shift -1 var)))))

  dhall_clj.ast.Annot
  (beta-normalize [this]
    (beta-normalize (:val this)))

  dhall_clj.ast.BoolT
  (beta-normalize [this] this)

  dhall_clj.ast.BoolLit
  (beta-normalize [this] this)

  dhall_clj.ast.BoolAnd
  (beta-normalize [{:keys [a b]}]
    (let [decide (fn [l r]
                   (if (instance? BoolLit l)
                     (if (:b l)
                       r
                       l)
                     (if (instance? BoolLit r)
                       (if (:b r)
                         l
                         r)
                       (if (judgmentally-equal l r)
                         l
                         (->BoolAnd l r)))))]
      (decide (beta-normalize a) (beta-normalize b))))

  dhall_clj.ast.BoolOr
  (beta-normalize [{:keys [a b]}]
    (let [decide (fn [l r]
                   (if (instance? BoolLit l)
                     (if-not (:b l)
                       r
                       l)
                     (if (instance? BoolLit r)
                       (if-not (:b r)
                         l
                         r)
                       (if (judgmentally-equal l r)
                         l
                         (->BoolAnd l r)))))]
      (decide (beta-normalize a) (beta-normalize b))))

  dhall_clj.ast.BoolEQ
  (beta-normalize [{:keys [a b]}]
    (let [decide (fn [l r]
                   (cond
                     (and (instance? BoolLit l) (:b l)) r
                     (and (instance? BoolLit r) (:b r)) l
                     (judgmentally-equal l r)           (->BoolLit true)
                     :else                              (->BoolEQ l r)))]
      (decide (beta-normalize a) (beta-normalize b))))

  dhall_clj.ast.BoolNE
  (beta-normalize [{:keys [a b]}]
    (let [decide (fn [l r]
                   (cond
                     (and (instance? BoolLit l) (not (:b l))) r
                     (and (instance? BoolLit r) (not (:b r))) l
                     (judgmentally-equal l r)                 (->BoolLit false)
                     :else                                    (->BoolNE l r)))]
      (decide (beta-normalize a) (beta-normalize b))))

  dhall_clj.ast.BoolIf
  (beta-normalize [{:keys [test then else]}]
    (let [decide (fn [test then else]
                   (cond
                     (instance? BoolLit test)       (if (:b test)
                                                      then
                                                      else)
                     (and (instance? BoolLit then)
                          (instance? BoolLit else)
                          (:b then)
                          (not (:b else)))          test
                     (judgmentally-equal then else) then
                     :else                          (->BoolIf test then else)))]
      (decide (beta-normalize test)
              (beta-normalize then)
              (beta-normalize else))))

  dhall_clj.ast.NaturalT
  (beta-normalize [this] this)

  dhall_clj.ast.NaturalLit
  (beta-normalize [this] this)

  dhall_clj.ast.NaturalFold
  (beta-normalize [this] this)

  dhall_clj.ast.NaturalBuild
  (beta-normalize [this] this)

  dhall_clj.ast.NaturalIsZero
  (beta-normalize [this] this)

  dhall_clj.ast.NaturalEven
  (beta-normalize [this] this)

  dhall_clj.ast.NaturalOdd
  (beta-normalize [this] this)

  dhall_clj.ast.NaturalToInteger
  (beta-normalize [this] this)

  dhall_clj.ast.NaturalShow
  (beta-normalize [this] this)

  dhall_clj.ast.NaturalPlus
  (beta-normalize [{:keys [a b]}]
    (let [decide (fn [l r]
                   (cond
                     (and (instance? NaturalLit l) (= 0 (:n l))) r
                     (and (instance? NaturalLit r) (= 0 (:n r))) l
                     (and (instance? NaturalLit l)
                          (instance? NaturalLit r))              (->NaturalLit (+ (:n l)
                                                                                  (:n r)))
                     :else                                       (->NaturalPlus l r)))]
      (decide (beta-normalize a) (beta-normalize b))))

  dhall_clj.ast.NaturalTimes
  (beta-normalize [{:keys [a b]}]
    (let [decide (fn [l r]
                   (cond
                     (and (instance? NaturalLit l) (= 1 (:n l))) r
                     (and (instance? NaturalLit r) (= 1 (:n r))) l
                     (and (instance? NaturalLit l) (= 0 (:n l))) (->NaturalLit 0)
                     (and (instance? NaturalLit r) (= 0 (:n r))) (->NaturalLit 0)
                     (and (instance? NaturalLit l)
                          (instance? NaturalLit r))              (->NaturalLit (* (:n l)
                                                                                  (:n r)))
                     :else                                       (->NaturalTimes l r)))]
      (decide (beta-normalize a) (beta-normalize b))))

  dhall_clj.ast.IntegerT
  (beta-normalize [this] this)

  dhall_clj.ast.IntegerLit
  (beta-normalize [this] this)

  dhall_clj.ast.IntegerShow
  (beta-normalize [this] this)

  IntegerToDouble
  (beta-normalize [this] this)

  dhall_clj.ast.DoubleT
  (beta-normalize [this] this)

  dhall_clj.ast.DoubleLit
  (beta-normalize [this] this)

  dhall_clj.ast.DoubleShow
  (beta-normalize [this] this)

  dhall_clj.ast.TextT
  (beta-normalize [this] this)

  dhall_clj.ast.TextLit
  (beta-normalize [this]
    (let [normalized-chunks (:chunks (map-chunks this beta-normalize))
          ;; We "bring down" one level possibly nested TextLit
          new-chunks (mapcat #(if (instance? TextLit %)
                                (:chunks %)
                                (list %))
                             normalized-chunks)]
      ;; If we have only an expr we return that
      (if (and (= 1 (count new-chunks))
               (not (string? (first new-chunks))))
        (first new-chunks)
        (->TextLit new-chunks))))

  dhall_clj.ast.TextAppend
  (beta-normalize [{:keys [a b]}]
    (let [compact (fn [chunks]
                    (loop [cs  chunks
                           acc nil
                           new []]
                      (if (seq cs)
                        (let [c (first cs)]
                          (if (string? c)
                            (recur (rest cs)
                                   (str acc c)
                                   new)
                            (recur (rest cs)
                                   nil
                                   (conj new (or acc "") c))))
                        (if-not acc
                          new
                          (conj new acc)))))
          empty-text? (fn [t]
                        (and (instance? TextLit t)
                             (every? #(and (string? %) (empty? %))
                                     (:chunks t))))
          decide (fn [l r]
                   (cond
                     (empty-text? l)             r
                     (empty-text? r)             l
                     (and (instance? TextLit l)
                          (instance? TextLit r)) (->TextLit
                                                   (compact
                                                     (concat (:chunks l) (:chunks r))))
                     :else                       (->TextAppend l r)))]
      (decide (beta-normalize a) (beta-normalize b))))

  dhall_clj.ast.ListT
  (beta-normalize [this] this)

  dhall_clj.ast.ListLit
  (beta-normalize [{:keys [type?] :as this}]
    (-> this
       (assoc :type?  (when type? (beta-normalize type?)))
       (update :exprs (partial map beta-normalize))))

  dhall_clj.ast.ListAppend
  (beta-normalize [{:keys [a b]}]
    (let [decide (fn [l r]
                   (cond
                     (and (instance? ListLit l)
                          (empty? l))            r
                     (and (instance? ListLit r)
                          (empty? r))            l
                     (and (instance? ListLit l)
                          (instance? ListLit r)) (update l :exprs concat (:exprs r))
                     :else                       (->ListAppend l r)))]
      (decide (beta-normalize a) (beta-normalize b))))

  dhall_clj.ast.ListBuild
  (beta-normalize [this] this)

  dhall_clj.ast.ListFold
  (beta-normalize [this] this)

  dhall_clj.ast.ListLength
  (beta-normalize [this] this)

  dhall_clj.ast.ListHead
  (beta-normalize [this] this)

  dhall_clj.ast.ListLast
  (beta-normalize [this] this)

  dhall_clj.ast.ListIndexed
  (beta-normalize [this] this)

  dhall_clj.ast.ListReverse
  (beta-normalize [this] this)

  dhall_clj.ast.OptionalT
  (beta-normalize [this] this)

  dhall_clj.ast.OptionalLit
  (beta-normalize [{:keys [type val?]}]
    ;; FIXME: attach meta here when creating the new nodes
    (beta-normalize
      (if val?
        (->Some val?)
        (->App (->None) type))))

  dhall_clj.ast.Some
  (beta-normalize [this]
    (update this :e beta-normalize))

  dhall_clj.ast.None
  (beta-normalize [this] this)

  dhall_clj.ast.OptionalFold
  (beta-normalize [this] this)

  dhall_clj.ast.OptionalBuild
  (beta-normalize [this] this)

  dhall_clj.ast.RecordT
  (beta-normalize [this]
    (update this :kvs
            #(->> % (map-vals beta-normalize) (into (sorted-map)))))

  dhall_clj.ast.RecordLit
  (beta-normalize [this]
    (update this :kvs
            #(->> % (map-vals beta-normalize) (into (sorted-map)))))

  dhall_clj.ast.UnionT
  (beta-normalize [this]
    (update this :kvs
            #(->> % (map-vals beta-normalize) (into (sorted-map)))))

  dhall_clj.ast.UnionLit
  (beta-normalize [this]
    (-> this
       (update :v beta-normalize)
       (update :kvs
               #(->> % (map-vals beta-normalize) (into (sorted-map))))))

  dhall_clj.ast.Combine
  (beta-normalize [{:keys [a b]}]
    (letfn [(decide [l r]
              (cond
                (and (instance? RecordLit l)
                     (empty? l))              r
                (and (instance? RecordLit r)
                     (empty? r))              l
                (and (instance? RecordLit l)
                     (instance? RecordLit r)) (->RecordLit (merge-with decide (:kvs l) (:kvs r)))
                :else                         (->Combine l r)))]
      (decide (beta-normalize a) (beta-normalize b))))

  dhall_clj.ast.CombineTypes
  (beta-normalize [{:keys [a b]}]
    (letfn [(decide [l r]
              (cond
                (and (instance? RecordT l)
                     (empty? l))            r
                (and (instance? RecordT r)
                     (empty? r))            l
                (and (instance? RecordT l)
                     (instance? RecordT r)) (->RecordT (merge-with decide (:kvs l) (:kvs r)))
                :else                       (->CombineTypes l r)))]
      (decide (beta-normalize a) (beta-normalize b))))

  dhall_clj.ast.Prefer
  (beta-normalize [{:keys [a b]}]
    (letfn [(decide [l r]
              (cond
                (and (instance? RecordLit l)
                     (empty? l))              r
                (and (instance? RecordLit r)
                     (empty? r))              l
                (and (instance? RecordLit l)
                     (instance? RecordLit r)) (->RecordLit (merge (:kvs l) (:kvs r)))
                :else                         (->Prefer l r)))]
      (decide (beta-normalize a) (beta-normalize b))))

  dhall_clj.ast.Merge
  (beta-normalize [{:keys [a b type?]}]
    (let [a'     (beta-normalize a)
          b'     (beta-normalize b)
          type?' (when type? (beta-normalize type?))]
      (if (and (instance? RecordLit a')
               (instance? UnionLit b')
               (contains? (:kvs a') (:k b')))
        (beta-normalize
          (->App (get (:kvs a') (:k b'))
                 (:v b')))
        (->Merge a' b' type?'))))

  dhall_clj.ast.Constructors
  (beta-normalize [this]
    (let [e' (beta-normalize (:e this))]
      (if (instance? UnionT e')
        (let [kts   (:kvs e')
              adapt (fn [[k v]]
                      (->Lam k v (->UnionLit k
                                             (->Var k 0)
                                             (dissoc kts k))))]
          (->RecordLit (map adapt kts)))
        (->Constructors e'))))

  dhall_clj.ast.Field
  (beta-normalize [{:keys [e k] :as this}]
    (let [e' (beta-normalize e)]
      (if (instance? RecordLit e')
        (if-let [v (get (:kvs e') k)]
          (beta-normalize v)
          (->Field (->RecordLit (map-vals beta-normalize (:kvs e'))) k))
        (assoc this :e e'))))

  dhall_clj.ast.Project
  (beta-normalize [{:keys [e ks] :as this}]
    (let [e' (beta-normalize e)]
      (if (instance? RecordLit e')
        (let [kvs (:kvs e')]
          (if (every? (fn [k] (contains? kvs k)) ks)
            (beta-normalize (->RecordLit (select-keys kvs ks)))
            (->Project (->RecordLit (map-vals beta-normalize kvs)) ks)))
        (assoc this :e e'))))

  dhall_clj.ast.ImportAlt
  (beta-normalize [this]
    (beta-normalize (:a this))))
