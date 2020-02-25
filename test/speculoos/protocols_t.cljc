(ns speculoos.protocols-t
  (:refer-clojure :exclude [num val])
  (:require #?(:cljs [cljs.spec.alpha :as s] :clj [clojure.spec.alpha :as s])
            #?(:cljs [cljs.core.match] :clj [clojure.core.match])
            #?(:clj  [clojure.test :refer [deftest]]
               :cljs [cljs.test :refer-macros [deftest]])
            [speculoos.utils :as u #?(:clj :refer :cljs :refer-macros) [is]]
            [speculoos.specs])
  (#?(:clj :require :cljs :require-macros)
   [speculoos.core :refer [deft defproto proto+]]))

(deft num [val])
(deft fork [a b])

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

(deft nested {a [b c]
              b {(? c) ::nested
                 d ::num}})
(defproto P5
          (pouet ::num [_ f]))