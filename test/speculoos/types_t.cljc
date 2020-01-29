(ns speculoos.types-t
  (:require #?(:cljs [cljs.spec.alpha :as s] :clj [clojure.spec.alpha :as s])
            [speculoos.utils :as u :refer [is]]
            [clojure.test :as test :refer [deftest]])
  (#?(:clj :require :cljs :require-macros)
   [speculoos.types :refer [deft]]
   [speculoos.specs :refer [cpred]]))

(deftest one

  (deft box [val])

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

  (deft myfun [f]
        clojure.lang.IFn
        (invoke [this x] ((:f this) x)))

  ;;You can pass protocols implementations as in a `defrecord` form

  (is 1 ((myfun identity) 1))
  )

(deftest two

  ;; shorthand syntax
  ;; a spec keyword can be given after a field name, the spec will be used at construction time to conform the given value

  (deft num [val ::int])

  (num 1)

  (comment (num :not-a-number)) ;; throws: invalid field value: :not-a-number is not a valid ::int

  ;; regular syntax
  ;; any specizable object or expression that return a spec can be assigned to a field with the help of the :- syntax

  (deft t2 [a :- integer?
            b :- string?])

  (is (t2 1 "io"))

  ;; if no positional constructor is needed you can use map field specification
  (deft t2' {a integer? ;; a is the name of the field and integer? is the corresponding spec
             b string?})

  ;; when specifying field using a map, you can add optional fields (validated only if present)
  (deft t2'' {a integer?
              b string?
              (? c) keyword?}) ;; optional fields are wrapped in a (? _) expression

  (t2'' :a 1 :b "aze") ;; c is not here, no problem
  (t2'' :a 1 :b "aze" :c :op) ;; :c is here and validated

  ;; mixed

  (deft t3 [a ::int
            b :- string?
            c]) ;; a field without validation

  (is (t3 1 "io" :anything))

  ;; wrapped syntax
  ;; if it looks more clear to you, you can wrap one or more field specifications in parentesis

  (deft t3' [(a ::int)
             (b :- string?)])

  )

(deftest coercion

  ;;Lets first define a spec `::int!` that will turn any number to an integer.

  ;;The `cpred` macro takes a function that can return `nil` if its argument cannot be conformed, or the argument coerced indicating success.

  ;;Here we check if the argument is a number and turn it to an integer if so

  (s/def ::int!
    (cpred  #(when (number? %) (int %))))

  ;;Now we've got a spec that can be used to coerce given field values.


  ;; regular syntax

  (deft num2 [val ::int!])

  (num2 2.4) ;;=> (num 2)

  (is (num2 1)
      (num2 1.1)
      (num2 1.9))


  )
