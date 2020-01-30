(ns speculoos.utils-t
  (:require [clojure.test :refer [deftest]]
            [speculoos.utils :as u :refer [is isnt f_ f1]]))

(deftest maps
  (is (= {:a 1} (u/rem-nil-vals {:a 1 :b nil :c nil})))
  (is (= {:a 2 :b 3} (u/map-vals inc {:a 1 :b 2}))))

(deftest mksym
  (is (= 'foobar
         (u/mksym :foo 'bar)
         (u/mksym "fo" 'ob "ar")
         (u/mksym "fo" :ob 'ar ""))))

(u/defn+ add [x & xs]
         (apply + x xs))

(deftest testing-defn+
  (is 6
      (add 1 2 3)
      ((add_ 2 3) 1) ;; underscore version, return a function waiting for first argument
      (add* 1 [2 3]) ;; applied version takes a collection as last argument
      ((add_* 2 [1 1 1]) 1))) ;; underscore-applied version

(deftest f_-and-f1
  (is 2
      ((f_ (+ _ _)) 1)
      ((f1 x (+ x x)) 1) ;; little macro for unary functions
      ((f1 {a :a} (+ a a)) {:a 1})))


