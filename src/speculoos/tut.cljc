(ns speculoos.tut
  (:require #?(:cljs [cljs.spec.alpha :as s] :clj [clojure.spec.alpha :as s])
            #_[speculoos.utils :as u :refer [is]])
  (#?(:clj :require :cljs :require-macros)
   [speculoos.utils :as u :refer [is]]
   [speculoos.core :refer [defc deft fm defm defspec spec cpred]]))

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

    (s/valid? ::box (box 1)))

;; we can add validation spec to our type declarations

(s/def ::int integer?)

;; shorthand syntax

(deft num [val ::int]) ;; validation

;; regular syntax

(deft t2 [a :- integer? ;; any object that implement 'specize can be used in spec position
          b :- string?])

(is (num 1))

(comment (num :not-a-number)) ;; invalid field value: :not-a-number is not a valid ::int

(is (t2 1 "io"))

;; if no positional constructor is needed you can use map field specification
(deft t2' {a integer? ;; a is the name of the field and integer? is the corresponding spec
           b string?})

(s/conform integer? 1)

(vals (t2' :a 1 :b "aze"))

;; optional fields
(macroexpand '(deft t2'' {a integer?
                          b string?
                          (? c) keyword?}))

;; when specifying field using a map, you can add optional fields (validated only if present)
(deft t2'' {a integer?
             b string?
             (? c) keyword?})

(t2'' :a 1 :b "aze")
(t2'' :a 1 :b "aze" :c :op)

(num 1)



;; coercion

;; lets first define a spec ::int! that will turn any number to an integer
;; the `cpred` macro takes a function that can return nil if its argument cannot be conformed, or the argument coerced indicating success
;; here we check if the argument is a number and turn it to an integer if so
(s/def ::int!
  (cpred #(when (number? %) (int %))))

(s/def ::int!
  (s/conformer
    (fn [x]

      (cond

        (number? x) (int x)

        (or (string? x)
            (keyword? x)
            (symbol? x))
        (int (#?(:cljs js/parseFloat :clj Float/parseFloat)
               (name x)))))))

(s/conform ::int! "aze")

;; shorthand syntax

(deft num2 [val ::int!]) ;; coercion

(is (num2 1)
    (num2 1.1)
    (num2 1.9)
    (num2 "1.2")
    (num2 :1.2)
    (num2 '1)
    (num2 :1))

(comment (num2 [])) ;; [] cannot be coerced by :parkaviz.utils.matches-tries/coerced-int

;; regular syntax

(deft t4 [a :- ::int!])

;; coercion and validation can mixed

(deft t5 [a ::int!
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

(defm coerced-sum [(x ::num2)
                   (y ::num)]
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
      [(x ::int!) (y ::int!)] (+ x y) ;; coercion pattern
      [x y & (xs ::int!)] (reduce add x (cons y xs))) ;; coerced variadic pattern

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

(macroexpand '(deft rect [f :- (s/nilable ::rect)]))
(deft rect [f :- (s/nilable ::rect)])

(s/def ::rec (s/tuple integer? (s/nilable ::rec)))
(s/valid? ::rec [1 nil])
(s/valid? ::rec [1 [2 [3 nil]]])





