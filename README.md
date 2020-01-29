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

``` clojure
;; keyword lenses
(is 1 (get {:a 1} :a))
(is {:a 2} (mut {:a 1} :a inc))
(is {:a 1 :b 1} (mut {:a 0 :b 2} :a inc :b dec))

;; indexes
(is 2 (get [1 2 3] 1))
(is [1 3 3] (mut [1 2 3] 1 inc))
(is [2 2 2] (mut [1 2 3] 0 inc 2 dec))
(is [1 2 [4 4]]
    (mut [1 2 [3 4]] [2 0] inc))

;; composition
;; vector denotes composition (left to right)
(is 1 (get {:a {:b 1}} [:a :b]))
(is 3 (get {:a {:b [1 2 3]}} [:a :b 2]))
(is {:a {:b 2}} (mut {:a {:b 1}} [:a :b] inc))
(is {:a {:b 2 :c 1}}
    (mut {:a {:b 1 :c 2}}
         [:a :b] inc
         [:a :c] dec))

(is {:a 3, :c {:d 3}}
    (mut {:a 1 :c {:d 2}}
         :a (fn [x] (+ x x x))
         [:c :d] inc))


;; functions
(is 1 (get 1 pos?))
(isnt (get 1 neg?))

(is {:a 0} (mut {:a 1} [:a pos?] dec))
(isnt (mut {:a 0} [:a pos?] dec))

;; or
(is (zero? (mut< 1
                 neg? inc
                 pos? dec)))
(is {:a 0}

    (mut< {:a 1}
          [:a pos?] dec
          [:a neg?] inc)

    (mut< {:a -1}
          [:a pos?] dec
          [:a neg?] inc))

(is {:a {:b 2, :c -1}}
    (mut {:a {:b 1 :c -1}}
         (< [:a :c pos?]
            [:a :b pos?])
         inc))

;; option
(is {:a {:b 1}}
    (mut {:a {:b 1}} (? [:a :z :b]) inc))

; non existant keys
(is {:a {:b {:c 42}}}
    (mut {} (path [:a :b :c]) (constantly 42)))

(is {:a {:b {:c 42}}}
    (put {} (path :a :b :c) 42)
    (put {} (path [:a :b :c]) 42)
    (put {} (path :a [:b :c]) 42)
    (mut {} (path [:a :b] :c) (constantly 42)))

(is {:b 1}
    (mut {} (path :b) (fnil inc 0)))

; matching values
(is "io"
    (get {:a "io"} [:a "io"]))

(isnt (get {:a "io"} [:a "iop"]))

;; builtins

;; keys
(is {:a 2 :b 3}
    (mut {:a 1 :b 2} :* inc))

(is '(1 2)
    (get {:a 1 :b 2} :*))

;; convertion
(is (/ 11 10)
    (mut 1 (convertion #(* % 10)
                       #(/ % 10))
         inc))

;; check
(is (pass
      {:a 1 :b "io" :p 1}
      [:a number? pos? (lfn inc)]
      [:b string?])
    {:a 2 ;; :a has been coerced (with the help of 'lfn
     :b "io"
     :p 1})
```

## Flow 

``` clojure 
;; the '> function let you chain some transformations over something

;; the first argument is the thing we want to transform
(is 1
    (> 0 inc)
    (> -1 inc inc))

;; the others arguments are any ITrans(able) object
;; clojure primitives have special behaviors in '>

;; vectors do left to right composition
(is 2
    (> 0 [inc inc])
    (> -1 inc [inc inc] []))

;; maps are treated as a non ordered sequence of map entries where
;; map entries are used to denote a lens based transformation
(is {:a 2}
    (> {:a 1} {:a inc}))

;; any lens can be used
(is {:a {:b [1 2 3]}}
    (> {:a {:b [1 2 2]}} {[:a :b 2] inc})
    ;; it can be nested
    (> {:a {:b [1 2 2]}} {:a {[:b 2] inc}})
    (> {:a {:b [1 2 2]}} {:a {:b {2 inc}}}))

(is 0
    (> 1 {pos? dec})
    (> 1 (link pos? dec)) ;; link creates a map-entry
    (> 0 {(l/? pos?) dec}))

;; other values are treated as constant functions
(is 42 (> {:some :thing} 42)) ;; here 42 is in transformation position and just return itself
(is "yop" (> {:some :thing} "yop"))

;; the '< function is like an 'or
;; like '> it takes the object of the transformation as first argument
;; and some transformations, that will be tried in order until the first succesful one
;; transformation semantics are the same as in '>
(is 0
    (< 0
       {neg? dec}
       {pos? inc}
       identity)
    (< -1
       {neg? inc}
       {pos? dec}
       identity)
    (< 1
       {neg? inc}
       {pos? dec}
       identity))


(is {:a {:b 2 :c -2}}
    (> {:a {:b 1 :c -1}}
       {:a {:b inc :c dec}})
    (> {:a {:b 1 :c -1}}
       [{:a {:b inc}}
        {:a {:c dec}}])
    (> {:a {:b 1 :c -1}}
       {:a [{:b inc} {:c dec}]}))

(is [1 2 4]
    (> [1 2 3] {2 inc}))

(is [1 2 [4 4]]
    (> [1 2 [3 4]] {2 {0 inc}})
    (> [1 2 [3 4]] {[2 0] inc}))

(is [1 2 [{:a 1} 4]]
    (> [1 2 [{:a 0} 4]] {[2 0 :a] inc}))

(is {:a {:b 2, :c -2}}
    (> {:a {:b 1 :c -1}}
       {:a {:b inc}
        [:a :c neg?] dec}))

(is {:a {:b {:c 42}}}
    (> {}
       (at [:a :b :c] 42))
    (> {}
       {(l/path :a :b :c) 42}))

(is (> {}
       (at [:a :b :c] 42 ;; this assoc 42 at path [:a :b :c]
           :d 'pouet
           [:e :f] '(1 2 3)
           [:a :b :c] inc ;; updates are executed sequentially so the previously assoced 42 value is available
           ))

    {:a {:b {:c 43}},
     :d 'pouet,
     :e {:f '(1 2 3)}}

    ;; when using map syntax, there is no garanty of order
    ;; here the equivalent of the previous form
    (> {}
       {(l/path [:a :b :c]) 42 ;; this assoc 42 at path [:a :b :c]
        (l/path :d) 'pouet
        (l/path [:e :f]) '(1 2 3)}
       ;; so our transformations, if depending on freshly assoced values, has to wait the next reduction step
       ;; in this case, since the value now exists, the key does not have to be wrapped in 'lenses/path
       {[:a :b :c] inc}))


(let [t (<_ {pos? inc}
            {neg? dec}
            nil)]
  (is (t 1) 2)
  (is (t -1) -2)
  (is (t 0) 0))


(is ((>_ inc inc) 1)
    ((>_* [inc inc]) 1)
    ((>_* inc dec [inc inc]) 1)
    (>* 1 inc dec [inc inc])
    (> 1 (>_ inc inc)))

(isnt (l/get 1 (l/! neg?))
      (> 1 (link neg? inc)))

(let [t (>_ {neg? inc}
            inc)]
  (is 1 (t -1))
  (isnt (t 1)))


(isnt (> 1 (u/guard neg?) inc))

(is (zero? (> -1 (u/guard neg?) inc)))


(is ((f_ (+ _ _)) 1)
    ((f1 x (+ x x)) 1)
    ((f1 {a :a} (+ a a)) {:a 1}))

;; using flow as a spec like mecanism

(> {:a 1 :b "io" :p 1}
   {[:a pos?] nil [:b string?] nil})

(> {:a :1 :b "io" :p 1}
   {[:a number? pos?] nil
    [:b string?] nil})
```