(ns speculoos.types-t
  (:require #?(:clj [clojure.spec.alpha :as s] :cljs [cljs.spec.alpha :as s])
            #?(:clj [clojure.spec.gen.alpha :as gen] :cljs [cljs.spec.gen.alpha :as gen])
            [speculoos.utils :as u #?(:clj :refer :cljs :refer-macros) [is]]
            #?(:clj  [clojure.test :refer [deftest]]
               :cljs [cljs.test :refer-macros [deftest]])
            [speculoos.specs])
  (#?(:clj :require :cljs :require-macros)
   [speculoos.types :refer [deft defc]]
   [speculoos.specs :refer [cpred]]))

;; creating a simple type with one field

(deft box [val])

(u/with-dotsyms
  (deftest one

    ;; It can be instanciated like this

    (is (box 1)) ;;=> (box 1)

    ;; It prints in a more concise way than default clojure record e.g =(box 1)=

    ;; We can access its field with normal clojure syntax.

    (is (:val (box 1))) ;;=> 1

    ;; A predicate is available too

    (is (box? (box 1))) ;;=> true

    ;; the ::box spec is defined too

    (is (s/conform ::box {:val 1})
        (box 1))

    (is (s/valid? ::box (box 1)))

    (is (every? box? (gen/sample (s/gen ::box) 1000)))

    ;; a map constructor is defined too (like with defrecord
    (is (box 1) (box.from-map {:val 1}))))

;;You can pass protocols implementations as in a `defrecord` form

#?(:clj (do (deft myfun [f]
                  clojure.lang.IFn
                  (invoke [this x] ((:f this) x)))

            (deftest impls
              (is 1 ((myfun identity) 1)))))

;; field validation ---------------------------------------------

;; defining a simple spec for tests

(s/def ::int integer?)

;; shorthand syntax --------

;; a spec keyword can be given after a field name, the spec will be used at construction time to conform the given value

(deft num [val ::int])

(deftest two

  (is (num 1))

  (comment (num :not-a-number)) ;; throws: invalid field value: :not-a-number is not a valid ::int

  )

;; regular syntax -----------

;; any specizable object or expression that return a spec can be assigned to a field with the help of the :- syntax

(deft t2 [a :- integer?
          b :- string?])

;; if no positional constructor is needed you can use map field specification
(deft t2' {a integer? ;; a is the name of the field and integer? is the corresponding spec
           b string?})

;; when specifying field using a map, you can add optional fields (validated only if present)
(deft t2'' {a integer?
            b string?
            (? c) keyword?}) ;; optional fields are wrapped in a (? _) expression

;; mixed

(deft t3 [a ::int
          b :- string?
          c]) ;; a field without validation

;; wrapped syntax
;; if it looks more clear to you, you can wrap one or more field specifications in parentesis

(deft t3' [(a ::int)
           (b :- string?)])

(u/with-dotsyms
  (deftest constructors
    ;; t2 instantiation (positional constructor
    (is (t2 1 "io")
        ;; map constructor
        (t2.from-map {:a 1 :b "io"}))
    ;; t2' has no positional constructor since it was defined using a map field spec
    ;; it can be instanciated like this
    (is (t2' :a 1 :b "io")
        (t2'.from-map {:a 1 :b "io"}))
    ;; t2'' can have a c field or not (if so it has to conform to the keyword? spec
    (is (t2'' :a 1 :b "aze")) ;; c is not here, no problem
    (is (t2'' :a 1 :b "aze" :c :op)) ;; :c is here and validated
    ;; t3 has a positional constructor and a c field that can be anything (no spec attached)
    (is (t3 1 "io" :anything))))


;; coercion ------------------------------------------------------------------

;;Lets first define a spec `::int!` that will turn any number to an integer.

;;The `cpred` macro takes a function that can return `nil` if its argument cannot be conformed, or the argument coerced indicating success.

;;Here we check if the argument is a number and turn it to an integer if so

(s/def ::int!
  (cpred #(when (number? %) (int %))))

;;Now we've got a spec that can be used to coerce given field values.

;; regular syntax

(deft num2 [val ::int!])

(deftest coercion

  (is (num2 2)
      (num2 2.4)) ;;=> (num 2)

  (is (num2 1)
      (num2 1.1)
      (num2 1.9)))

;; nested types

(macroexpand '(deft t4 {a [b c] b {c ::int! d string? e [f ::int]}}))

;; defc -----------------------------------------------------------------------

;; a simple type with two unvalidated fields (for the exemple)
(deft fork [a b])

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

(deftest testing-defc

  (is (duo (num 1) (num 2))) ;;=> (duo 1 2)
  (is (duo (fork :a :b) (fork :c :d))) ;;=> (duo :a :d)
  (is (duo :what :ever)) ;=> (duo :what :ever)
  (is (duo (num 1) (num 2) 3)) ;=> (duo 3 3)
  (is (duo :iop (num 1) (num 2)))) ;=> (duo :iop 3)
