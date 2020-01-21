# Speculoos

a collection of low level macros using core.match and clojure.spec 

## Usage 

add this to your `deps.edn`:

```univalence/speculoos {:mvn/version "0.1.0-SNAPSHOT"}```

or if you are using leiningen or boot: 

```[univalence/speculoos "0.1.0-SNAPSHOT"]```

## specs 

With clojure.spec, when you are creating a spec, or reifying the spec protocol, what you get back is an opaque object, not allowing implementation sharing or composition.  
With the `SpecImpl` record you have something that behaves exactely like a Spec but expose its implementations

speculoos.core exposes a `spec` function that let you create a `SpecImpl` record 

``` clojure

```

 

## `deft`

`deft` defines a new type, it is a thin wrapper around defrecord

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

It comes with validation and coercion capabilities 

### validation 

``` clojure
;; shorthand syntax

(deft num [val ::int]) ;; validation

(num 1)

(num :not-a-number) ;; throws: invalid field value: :not-a-number is not a valid ::int

;; regular syntax

(deft t2 [a :- integer? ;; any object that implement 'specize can be used in spec position
          b :- string?])

(is (t2 1 "io"))

;; mixed
(deft t3 [a ::int
          b :- string?
          c]) ;; a field without validation

(is (t3 1 "io" :anything))

```

### Coercion 

```
;;coercion regular syntax

(deft num2 [val :< int])

(num2 2.4) ;;=> (num 2)

```


## `fm` and `defm`

## `defc`
