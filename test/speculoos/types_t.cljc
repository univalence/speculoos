(ns speculoos.types-t
  (:refer-clojure :exclude [num val])
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
  (is (box 1) (#?(:cljs box.from-map :clj box/from-map) {:val 1})))

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
    ;; t2 instantiation (positional constructor)
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
    (is (t3 1 "io" :anything))

    ;; extra constructor arities

    ;; for conveniance positional types that have more than one fields have an extra arity 1
    ;; letting you pass either a map or a sequence
    (is (t2 [1 "io"])
        (t2 {:a 1 :b "io"})
        (t2 1 "io"))

    ;; you can pass extra keys like this
    (is (t2 1 "io" :extra-field :extra-val) ;; a series of keyvalues following the positional fields
        (t2 1 "io" {:extra-field :extra-val}) ;; a map following the positional fields
        (t2 [1 "io" :extra-field :extra-val]) ;; the 2 above exemple works also when using applied syntax
        (t2 [1 "io" {:extra-field :extra-val}])
        (t2 {:a 1 :b "io" :extra-field :extra-val})) ;; a map containing required fields plus some extra fields

    ;; same for non positional types
    (is (t2' {:a 1 :b "io"})
        (t2' :a 1 :b "io"))

    (is (t2' {:a 1 :b "io" :extra-field :extra-val}) ;; with extended map
        (t2' :a 1 :b "io" :extra-field :extra-val) ;; with extra fields
        (t2' :a 1 :b "io" {:extra-field :extra-val}) ;; with extra map arg
        (t2' [:a 1 :b "io" :extra-field :extra-val]) ;; applied
        (t2' [:a 1 :b "io" {:extra-field :extra-val}])
        )
    ))


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

;; nested types ---------------------------------------------------------------

;; types can be nested

(deft t4
      {a [b c]
       b {c ::int!
          d string?
          e [f ::int
             g :- string?]}})

;; if a field has a spec in the form of a literal map or literal vector
;; it is interpreted as a subtype

;; in the preceding exemple those subtypes will be defined:

;; (deft t4.a [b c])
;; (deft t4.b.e [f ::int g :- string?])
;; (deft t4.b
;;       {c ::int!
;;        d string?
;;        e ::t4.b.e})

;; then t4 can be defined like this
;; (deft t4
;;       {a ::t4.a
;;        b ::t4.b})

(u/with-dotsyms

  (deftest nested-tests
    ;; subconstructors
    (is (t4.a 1 2)
        (t4.a [1 2])
        (t4.a.from-map {:b 1 :c 2}))

    (is (t4.a? (t4.a 1 2))
        (t4.a? (t4.a [1 2]))
        (t4.a? (t4.a.from-map {:b 1 :c 2})))

    (is (t4.b :c 1 :d "aze" :e {:f 1 :g "baz"}) ;; c value is coerced to int
        (t4.b {:c 1.9 :d "aze" :e [1 "baz"]})
        (t4.b.from-map {:c 1.2 :d "aze" :e {:f 1 :g "baz"}}))

    (is (t4.b? (t4.b :c 1.1 :d "aze" :e {:f 1 :g "baz"})))
    (is (t4.b? (t4.b {:c 1.2 :d "aze" :e [1 "baz"]})))

    ;; constructor
    (is (t4 :a (t4.a 1 2) :b (t4.b :c 1 :d "aze" :e {:f 1 :g "baz"}))
        (t4 :a [1 2] :b {:c 1.92 :d "aze" :e {:f 1 :g "baz"}})
        (t4 {:a [1 2] :b {:c 1.21 :d "aze" :e [1 "baz"]}})
        (t4.from-map {:a [1 2] :b {:c 1.2 :d "aze" :e [1 "baz"]}}))))

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

