(ns speculoos.patterns-t
  (:require #?(:cljs [cljs.spec.alpha :as s] :clj [clojure.spec.alpha :as s])
            [clojure.test :refer [deftest]]
            [speculoos.utils :as u :refer [is]])
  (#?(:clj :require :cljs :require-macros)
   [speculoos.types :refer [deft]]
   [speculoos.specs :refer [cpred]]
   [speculoos.patterns :refer [defm fm]]))

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

(s/def ::int integer?)

(s/def ::int!
  (cpred #(when (number? %) (int %))))

(deft num2 [val ::int!])

(defm coerced-sum [(x ::num2) ;; coercion pattern (shorthand syntax)
                   (y ::num)] ;; validation pattern (shorthand syntax)
      (num (+ (:val x) (:val y))))

(deftest two

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

      [(num 1) 2 3]))

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
      [0 x] x ;; patterns can match any values
      [x 0] x
      [(x ::int!) (y ::int!)] (+ x y) ;; coercion pattern
      [x y & (xs ::int!)] (reduce add x (cons y xs))) ;; coerced variadic pattern

(deftest four
  (is 3 (add 1 2))
  (is 10 (add 1 2 3 4))
  (is 10 (add 1 2 3 4.2))

  ;; the following will throw because it does not match the return spec
  (comment (add 0 1 2 3 4.1)
           (add 1 1.2)))

;;speculoos.tut/add:
;invalid return value: 2.2 is not a valid :parkaviz.scratch.matches-tries/int

;; You can put several variadic patterns

(defm add2
      [x y & nil] (+ x y)
      [x y & xs] (apply add2 (add2 x y) xs))

(deftest five
  (is 18 (add2 3 4 5 6)))

