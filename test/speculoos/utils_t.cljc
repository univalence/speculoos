(ns speculoos.utils-t
  (:require [clojure.test :refer :all]
            [speculoos.utils :as u]))

(deftest maps
  (is (= {:a 1} (u/rem-nil-vals {:a 1 :b nil :c nil})))
  (is (= {:a 2 :b 3} (u/map-vals inc {:a 1 :b 2}))))

(deftest mksym
  (is (= 'foobar
         (u/mksym :foo 'bar)
         (u/mksym "fo" 'ob "ar")
         (u/mksym "fo" :ob 'ar ""))))


