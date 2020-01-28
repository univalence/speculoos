# Speculoos

a collection of low level macros using core.match and clojure.spec 

## Usage 

add this to your `deps.edn`:

```univalence/speculoos {:mvn/version "0.1.0-SNAPSHOT"}```

or if you are using leiningen or boot: 

```[io.univalence/speculoos "0.1.0-SNAPSHOT"]```

## requiring

All further exemple asumes the following ns declaration 

clojure: 
``` clojure
(ns speculoos.tut
  (:require [clojure.spec.alpha :as s]
            [speculoos.utils :as u :refer [is]]
            [speculoos.core :refer [defc deft fm defm defspec spec cpred]]))
```

clojurescript:
``` clojure
(ns speculoos.tut
  (:require [cljs.spec.alpha :as s]
            [speculoos.utils :as u :refer [is]]
            [speculoos.core :refer-macros [defc deft fm defm defspec spec cpred]]))
```

cljc:
``` clojure 
(ns speculoos.tut
  (:require #?(:cljs [cljs.spec.alpha :as s] :clj [clojure.spec.alpha :as s])
            [speculoos.utils :as u :refer [is]])
  (#?(:clj :require :cljs :require-macros)
   [speculoos.core :refer [defc deft fm defm defspec spec cpred]])) 
```

## `deft`

`deft` defines a new type, it is a thin wrapper around `defrecord`

``` clojure

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
```

You can pass protocols implementations as in a `defrecord` form

``` clojure
(deft myfun [f]
   clojure.lang.IFn
   (invoke [this x] ((:f this) x)))

(is 1 ((myfun identity) 1))
```

It comes with validation and coercion capabilities. 

### conformed fields 

``` clojure

;; shorthand syntax
;; a spec keyword can be given after a field name, the spec will be used at construction time to conform the given value

(deft num [val ::int]) 

(num 1)

(num :not-a-number) ;; throws: invalid field value: :not-a-number is not a valid ::int

;; regular syntax
;; any specizable object or expression that return a spec can be assigned to a field with the help of the :- syntax

(deft t2 [a :- integer? 
          b :- string?])

(is (t2 1 "io"))

;; mixed

(deft t3 [a ::int
          b :- string?
          c]) ;; a field without validation

(is (t3 1 "io" :anything))

;; wrapped syntax
;; if it looks more clear to you, you can wrap one or more field specifications in parentesis

(deft t3' [(a ::int)
           (b :- string?)])
```

### Coercion 

Lets first define a spec `::int!` that will turn any number to an integer.  

The `cpred` macro takes a function that can return `nil` if its argument cannot be conformed, or the argument coerced indicating success.  

Here we check if the argument is a number and turn it to an integer if so

``` clojure
(s/def ::int!
  (cpred  #(when (number? %) (int %))))
  
```

Now we've got a spec that can be used to coerce given field values.

``` clojure
;; regular syntax

(deft num2 [val ::int!])

(num2 2.4) ;;=> (num 2)

(is (num2 1)
    (num2 1.1)
    (num2 1.9))
```


## `fm` and `defm`

`defm` is defining a pattern matched function, where user types (defined with `deft` or `defc`) can be matched/destructured

``` clojure
(defm sum
      [(num x) (num y)] (num (+ x y))
      [(fork x y) z] (sum (sum x y) z)
      [x (fork y z)] (sum x (sum y z)))

(is (sum (num 1) (num 2))
    (num 3))

(is (num 10)
    (sum (fork (num 3) (fork (num 1) (num 2)))
         (num 4)))

(defm coerced-sum [(x ::num2) ;; coercion pattern (shorthand syntax)
                   (y ::num)] ;; validation pattern (shorthand syntax)
      (num (+ (:val x) (:val y))))

(is (coerced-sum {:val 1.3} (num 2))
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
(is 10 (add 1 2 3 4.2))

;; the following will throw because it does not match the return spec
(comment (add 0 1 2 3 4.1)
         (add 1 1.2))
;;speculoos.tut/add:
;invalid return value: 2.2 is not a valid :parkaviz.scratch.matches-tries/int

;; You can put several variadic patterns

(defm add2
      [x y & nil] (+ x y)
      [x y & xs] (apply add2 (add2 x y) xs))

(is 18 (add2 3 4 5 6))
```

## `defc`

`defc` defines a new type, like `deft`.

Along with a pattern matched constructor function:

``` clojure
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

```

## specs 

With clojure.spec, when you are creating a spec, or reifying the spec protocol, what you get back is an opaque object, not allowing implementation sharing or composition.  
With the `SpecImpl` record you have something that behaves exactely like a spec but expose its implementations.  

speculoos.core exposes a `spec` macro that let you create a `SpecImpl` record.  
It works exactly like `clojure.spec.alpha/spec` but wraps the result in a `SpecImpl` instance

speculos.core also bring the `cpred` macro which is handy to build coercion specs.

## Lenses 

TODO

## Flow 

TODO