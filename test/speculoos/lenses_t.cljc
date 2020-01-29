(ns speculoos.lenses-t
  (:refer-clojure :exclude [get <])
  (:require [clojure.test :refer [deftest]]
            [speculoos.utils :refer [is isnt]]
            [speculoos.lenses :refer [get put mut mut< < path pass ? convertion lfn]]))

(deftest keyword-lenses
  (is 1 (get {:a 1} :a))
  (is {:a 2} (mut {:a 1} :a inc))
  (is {:a 1 :b 1} (mut {:a 0 :b 2} :a inc :b dec)))

(deftest indexes-leses
  (is 2 (get [1 2 3] 1))
  (is [1 3 3] (mut [1 2 3] 1 inc))
  (is [2 2 2] (mut [1 2 3] 0 inc 2 dec))
  (is [1 2 [4 4]]
      (mut [1 2 [3 4]] [2 0] inc)))

(deftest composition
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
           [:c :d] inc)))


(deftest functions
  (is 1 (get 1 pos?))
  (isnt (get 1 neg?))

  (is {:a 0} (mut {:a 1} [:a pos?] dec))
  (isnt (mut {:a 0} [:a pos?] dec)))

(deftest branching

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
           inc)))

(deftest option
  (is {:a {:b 1}}
      (mut {:a {:b 1}} (? [:a :z :b]) inc)))

(deftest non-existant-keys

  (is {:a {:b {:c 42}}}
      (mut {} (path [:a :b :c]) (constantly 42)))

  (is {:a {:b {:c 42}}}
      (put {} (path :a :b :c) 42)
      (put {} (path [:a :b :c]) 42)
      (put {} (path :a [:b :c]) 42)
      (mut {} (path [:a :b] :c) (constantly 42)))

  (is {:b 1}
      (mut {} (path :b) (fnil inc 0))))

(deftest matching-values
  (is "io"
      (get {:a "io"} [:a "io"]))

  (isnt (get {:a "io"} [:a "iop"])))

(deftest builtins
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
       :p 1}))
