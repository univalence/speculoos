(ns speculoos.patterns-t
  (:require #?(:cljs [cljs.spec.alpha :as s] :clj [clojure.spec.alpha :as s])
            #?(:cljs [cljs.core.match] :clj [clojure.core.match])
            #?(:clj  [clojure.test :refer [deftest]]
               :cljs [cljs.test :refer-macros [deftest]])
            [speculoos.utils :as u #?(:clj :refer :cljs :refer-macros) [is]]
            [speculoos.specs])
  (#?(:clj :require :cljs :require-macros)
   [speculoos.types :refer [deft]]
   [speculoos.specs :refer [cpred]]
   [speculoos.patterns :refer [defm fm defproto proto+]]))

;; defining two simple types for exemples

(deft num [val])
(deft fork [a b])

;; `defm` is defining a pattern matched function, where user types (defined with `deft` or `defc`) can be matched/destructured

(defm sum
      [(num x) (num y)] (num (+ x y))
      [(fork x y) z] (sum (sum x y) z)
      [x (fork y z)] (sum x (sum y z)))

(deftest one

  (is (sum (num 1) (num 2))
      (num 3))

  (is (num 10)
      (sum (fork (num 3) (fork (num 1) (num 2)))
           (num 4))))

;; anonymous form

(deftest testing-fm

  (let [f (fm [x y] :a
              [x y z] :b)]
    (and (is :a (f 1 2))
         (is :b (f 1 2 3))))

  ;; Sometimes you want the whole structure, not destructured

  (is ((fm [(num? x) (fork y z)] [x y z])
       ;; (num? x) is what I call a type-predicate pattern
       ;; it binds x to the whole structure
       (num 1)
       (fork 2 3))

      [(num 1) 2 3])

  ;; In fact any symbol ending with '? in verb position is interpreted as a predicate pattern
  ;; (it has to be a symbol ending with '? to be recognized as this)
  (is "1&foo"
      ((fm [(integer? x) (string? y)] (str x "&" y)) 1 "foo"))

  ;; like defm, fm can of course have several cases
  (let [f (fm [(integer? x) (string? y)] (str x "&" y)
              [(integer? x) y] (list x y)
              [x y] :something-else)]
    (is (list 1 2) (f 1 2))
    (is "1&foo" (f 1 "foo"))
    (is :something-else (f :a :b)))
  )

;; defining two specs for exemples

;; simple
(s/def ::int integer?)

;; coercive int spec
(s/def ::int!
  (cpred #(when (number? %) (int %))))

(deft num2 [val ::int!])

;; spec based matching can be done like this
;; spec pattern have two forms:
;; (pattern spec-keyword)
;; (pattern :- spec-expression)
(defm sum2
      [(x ::num2) (y ::num)] ;; form 1
      (num (+ (:val x) (:val y)))

      [(x :- integer?) y] ;; form 2
      (sum2 (num2 x) y)
      )

(comment
  (macroexpand '(speculoos.patterns/fm
                  sum2
                  [(x :speculoos.patterns-t/num2) (y :speculoos.patterns-t/num)]
                  (num (+ (:val x) (:val y)))
                  [(x :- integer?) y]
                  (sum2 (num2 x) y)))

  (binding [*cljs?* true]
    (cljs.core.match/match
      [G__77078 G__77079]
      [(x ::num2) (y ::num)]
      (num (+ (:val x) (:val y)))
      [(x :- integer?) y]
      (+ (num2 x) y))))


(deftest spec-patterns

  (is (sum2 {:val 1.3} (num 2)) ;; coerced, case 1
      (sum2 (num2 1) (num 2)) ;; matched case 1
      (sum2 1 (num 2)) ;; case 2
      (num 3)) ;; result

  ;; you can put any binding pattern in a spec pattern
  (let [f (fm [({:val (pos? x)} ::num)] x ;; if argument is a ::num, we destructure it with map literal pattern, then check that the value of the :val field is positive
              [_] :pouetpouet)]
    (is :pouetpouet (f (num -1)))
    (is 1 (f (num 1))))

  )

;; defm can have several arities

(defm myfun
      [0 x] :a
      [1 x] :b
      [x y] :c
      [0 x y] :d
      [x y z g] :e)

(deftest three
  (is :a (myfun 0 42))
  (is :b (myfun 1 'iop))
  (is :c (myfun 2 3))
  (is :d (myfun 0 :foo :bar))
  (is :e (myfun 0 :foo :bar :baz)))

;; Even variadic

(defm add
      ::int ;; a return spec can be given before clauses
      [x] x
      [0 x] x ;; as in core.match, patterns can match any values
      [x 0] x
      [(x ::int!) (y ::int!)] (+ x y) ;; coercion pattern
      [x y & (xs ::int!)] (reduce add x (cons y xs))) ;; coerced variadic pattern

(deftest testing-add
  (is 3 (add 1 2))
  (is 10 (add 1 2 3 4))
  (is 10 (add 1 2 3 4.2))

  ;; the following will throw because it does not match the return spec
  ;; the 3 first cases are not coercing input to integer, this is why it fails
  (comment (add 0 1.1)
           (add 1.2))
  ;;speculoos.tut/add:
  ;invalid return value: 2.2 is not a valid :parkaviz.scratch.matches-tries/int
  )

(deftest core-match-builtin-patterns
  ;; defm and fm can take any builtin core.match supported pattern
  (let [f (fm [([0 & xs] :seq)] :case1
              [(:or 1 -1)] :case2
              [_] :case3)]
    (is :case1 (f (list 0 1 2 3)))
    (is :case2 (f 1))
    (is :case3 (f :iop))))

;; You can put several variadic patterns

(defm add2
      :- number? ;; return spec can be defined with :- if the spec is not a qualified-keyword
      [x] x
      [x y & nil] (+ x y)
      [x y & xs] (apply add2 (add2 x y) xs))

(deftest five
  (is 18
      (add2 3 4 5 6)
      (add2 18))

  (comment (add2 :io)) ;; throws because of violated return spec
  )

;; defproto is an extension of core/defprotocol
;; it can be used exactly like defprotocol
(defproto P1
          (p1-1 [a]))

(proto+ P1
        #?(:clj String :cljs string)
        (p1-1 [a] :m1))

(defproto P2
          (p2-1 [a] [a b])
          (p2-2 [a b c]))

(proto+ P2

        #?(:clj String :cljs string)
        (p2-1 ([a] :str-p2-1-arity-1)
              ([a b] :str-p2-1-arity-2))
        (p2-2 [a b c] :str-p2-2)

        #?(:clj Object :cljs default)
        (p2-1 ([a] :obj-p2-1-arity-1)
              ([a b] :obj-p2-1-arity-2))
        (p2-2 [a b c] :obj-p2-2))

(deftest defproto-simple
  (is :m1 (p1-1 ""))
  (is :str-p2-1-arity-1 (p2-1 ""))
  (is :str-p2-1-arity-2 (p2-1 "" 42))
  (is :str-p2-2 (p2-2 "" 1 2))
  (is :obj-p2-1-arity-1 (p2-1 []))
  (is :obj-p2-1-arity-2 (p2-1 () 42))
  (is :obj-p2-2 (p2-2 :p 1 2)))

;; but it can specify its return spec

(defproto P3
          (p3-1 string? [a])) ;; protocol signatures can take a spec imediately after the method name (here 'string?)

(proto+ P3
        #?(:clj String :cljs string)
        (p3-1 [a] a)
        #?(:clj Object :cljs default)
        (p3-1 [_] "p3-1 default impl")
        #?(:clj Number :cljs number)
        (p3-1 [_] :faulty-implementation)) ;; it could be nice to get a compile time error here...

(deftest defproto-return-spec
  (is "p3-1 default impl" (p3-1 []))
  (is "yop" (p3-1 "yop"))
  (is ::catched (try (p3-1 1) (catch #?(:clj Exception :cljs js/Error) e ::catched)))) ;; since this faulty implementation return a keyword, it throws

;; protocols implementation can contain patterns

(defproto P4
          (p4-1 string? [a] [a b]))

(proto+ P4
        #?(:clj String :cljs string)
        (p4-1 ([a] a)
              ([a (string? b)] (str a b))))

(deftest proto-patterns
  (is "a" (p4-1 "a"))
  (is "ab" (p4-1 "a" "b"))
  (is ::catched (try (p4-1 "a" 1) (catch #?(:clj Exception :cljs js/Error) e ::catched))))

;; cases do not have to be wrapped in parens, and you can specify several patterns per arity

(defproto P4'
          (p4-1' string? [a] [a b]))

(proto+ P4'
        #?(:clj String :cljs string)
        (p4-1'
          [a] a
          ;; three patterns for arity 2
          [a (string? b)] (str a b)
          [a (number? b)] (str a "-num-" b)
          [a (num b)] (p4-1' a b))) ;; using the 'num type (defined on top of this file, and its generated destructuring pattern)

(deftest proto-syntax
  (is "a" (p4-1' "a"))
  (is "ab" (p4-1' "a" "b"))
  (is "a-num-1" (p4-1' "a" 1) (p4-1' "a" (num 1))))

