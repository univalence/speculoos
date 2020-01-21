(ns speculoos.walkthrough
  (:require #?(:cljs [cljs.spec.alpha :as s]
               :clj  [clojure.spec.alpha :as s])
            #?(:clj  [clojure.core.match]
               :cljs [cljs.core.match])
            [speculoos.specs :as ss]
            [speculoos.utils :as u :refer [is]])
  #?(:clj
     (:require
       [speculoos.types :refer [defc deft]]
       [speculoos.patterns :refer [fm defm]])
     :cljs
     (:require-macros
       [speculoos.types :refer [defc deft]]
       [speculoos.patterns :refer [fm defm]])))

;; defining a simple type

(do (deft box [val])

    ;; It can be instanciated like this

    (box 1) ;;=> (box 1)

    ;; It prints in a more concise way than default clojure record e.g =(box 1)=

    ;; We can access its field with normal clojure syntax.

    (:val (box 1)) ;;=> 1

    ;; A predicate is available too

    (box? (box 1)) ;;=> true

    ;; the ::box spec is defined too

    (is (s/conform ::box {:val 1})
        (box 1))

    (s/valid? ::box (box 1))

    #_(ss/gen1 ::box))

;; we can add validation spec to our type declarations

(s/def ::int integer?)

;; shorthand syntax

(deft num [val ::int]) ;; validation

;; regular syntax

(deft t2 [a :- integer?
          b :- string?])

(is (num 1))

(comment (num :not-a-number)) ;; invalid field value: :not-a-number is not a valid ::int


(is (t2 1 "io"))

;; can be mixed
(deft t3 [a ::int
          b :- string?
          c]) ;; a field without validation

(is (t3 1 "io" :anything))

;; coercion

(ss/defspec coerced-int
            ;; conform function
            (fn [_ x]
              (cond
                (integer? x) x
                (or (string? x) (keyword? x))
                (u/parse-int (name x))
                :else ::s/invalid))
            ;; gen function
            (fn [& _] (s/gen integer?))
            ;; descritption
            'int)

;; shorthand syntax

(deft num2 [(::coerced-int val)]) ;; coercion

(is (num2 1)
    (num2 "1")
    (num2 :1))

(comment (num2 [])) ;; [] cannot be coerced by :parkaviz.utils.matches-tries/coerced-int

;; regular syntax

(deft t4 [a :< ::coerced-int])

;; coercion and validation can mixed

(deft t5 [(::coerced-int a)
          b :- integer?
          (c :- string?)]) ;; regular syntax can be wrapped in parens if more readable

(is (t5 "1" 2 "io")
    (t5 1 2 "io"))

;; A type can have several fields

(deft fork [left right])

;; And take protocols implementation as defrecord do:

#?(:clj (do (deft myfun [f]
                  clojure.lang.IFn
                  (invoke [this x] ((:f this) x)))

            (is 1 ((myfun identity) 1))))

;; each type declaration define a spec along with the type
;; here we use our previously defined ::num spec
(deft numfork [a ::num
               b ::num])

(is (numfork (num 1) (num 2)))
(comment (numfork 1 2)) ;; throws

(is (s/conform ::num {:val 1})
    (num 1))

;; -------------------------------------------

;; defm is defining a pattern matched function.
;; Where user types (defined with deft or defc) can be matched/destructured

(defm sum
      [(num x) (num y)] (num (+ x y))
      [(fork x y) z] (sum (sum x y) z)
      [x (fork y z)] (sum x (sum y z)))

(is (sum (num 1) (num 2))
    (num 3))

(is (num 10)
    (sum (fork (num 3) (fork (num 1) (num 2)))
         (num 4)))

(defm coerced-sum [(::num2 x) ;; coercion pattern (shorthand syntax)
                               (y ::num)] ;; validation pattern (shorthand syntax)
                  (num (+ (:val x) (:val y))))

(is (coerced-sum {:val "1"} (num 2))
    (num 3))

(defm coerced-sum2 [(x :< ::num2) ;; coercion pattern
                    (y :- ::num)] ;; validation pattern
      (num (+ (:val x) (:val y))))

(is (coerced-sum2 {:val "1"} (num 2))
    (num 3))

;; anonymous form

(let [f (fm [x y] :a
            [x y z] :b)]
  (and (is :a (f 1 2))
       (is :b (f 1 2 3))))

(is ((fm [(num? x) (fork y z)] [x y z])
     ;; (num? x) is what I call a type-predicate pattern
     ;; it binds x to the whole structure
     (num 1)
     (fork 2 3))

    [(num 1) 2 3])

;; defm can have several arities

(defm myfun
      [0 x] :a
      [1 x] :b
      [x y] :c
      [0 x y] :d
      [x y z g] :e)

(is :a (myfun 0 42))
(is :b (myfun 1 'iop))
(is :c (myfun 2 3))
(is :d (myfun 0 :foo :bar))
(is :e (myfun 0 :foo :bar :baz))

;; Even variadic

(s/def ::ints (s/coll-of ::int))

(defm add
      ::int ;; a return spec can be given before clauses
      [x] x
      [0 x] x ;; patterns can match any values
      [x 0] x
      [(::coerced-int x) (::coerced-int y)] (+ x y) ;; coercion pattern
      [x y & (::coerced-int xs)] (reduce add x (cons y xs))) ;; coerced variadic pattern

(is 3 (add 1 2))
(is 10 (add 1 2 3 4))
(is 10 (add 1 2 3 "4"))

;; the following will throw because it does not match the return spec
(comment (add 0 1 2 3 4.1)
         (add 1 1.2))
;;parkaviz.scratch.matches-tries/add:
;invalid return value: 2.2 is not a valid :parkaviz.scratch.matches-tries/int

;; You can put several variadic patterns

(defm add2
      [x y & nil] (+ x y)
      [x y & xs] (apply add2 (add2 x y) xs))

(is 18 (add2 3 4 5 6))

;; defc defines a new type, like deft.
;; Along with a pattern matched constructor function:

(defc duo [a b] ;; this is the same as deft, a and b are the record fields
      ;; constructor cases
      ;; each case returns the fields values
      [(num x) (num y)] [x y] ;; here x and y will be bound to a and b fields
      [(fork x _) (fork _ y)] (vector x y)
      [x y] (list x y)
      ;; the constructor can have several arities as long as it returns the required fields values
      [(num x) (num y) z] {:a (+ x y) :b z}
      [x (num y) (num z)] (assoc {:a x} :b (+ y z)))

(duo (num 1) (num 2)) ;;=> (duo 1 2)
(duo (fork :a :b) (fork :c :d)) ;;=> (duo :a :d)
(duo :what :ever) ;=> (duo :what :ever)
(duo (num 1) (num 2) 3) ;=> (duo 3 3)
(duo :iop (num 1) (num 2))

;; Using fm in protocol declaration




