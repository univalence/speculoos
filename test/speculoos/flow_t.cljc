(ns speculoos.flow-t
  (:refer-clojure :exclude [< >])
  (:require [clojure.test :as t #?(:clj :refer :cljs :refer-macros) [deftest run-tests]]
            [speculoos.utils :refer [is isnt]]
            [speculoos.flow :refer [> >* >_ >_* < <* <_ <_* at link]]
            [speculoos.lenses :as l]
            [speculoos.utils :as u #?(:clj :refer :cljs :refer-macros) [f_ f1]]))

(deftest all

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

  ;; nil is the identity transformation
  (is 1 (> 1 nil nil))

  ;; other values are treated as constant functions
  (is 42
      (> {:some :thing} 42)) ;; here 42 is in transformation position and just return itself

  (is "yop"
      (> {:some :thing} "yop"))

  (is {:a {:b 1}}
      (> {:a {:b "hello"}}
         {[:a :b] 1})) ;; we are natigating to [:a :b] then apply 1 as a transformation (wich return itself)

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


  ;; several ways to update several things at once in our datastructure
  (is {:a {:b 2 :c -2}}
      (> {:a {:b 1 :c -1}}
         {:a {:b inc :c dec}})
      (> {:a {:b 1 :c -1}}
         [{:a {:b inc}}
          {:a {:c dec}}])
      (> {:a {:b 1 :c -1}}
         {:a [{:b inc} {:c dec}]}))

  ;; index based manipulation
  (is [1 2 4]
      (> [1 2 3] {2 inc}))

  (is [1 2 [4 4]]
      (> [1 2 [3 4]] {2 {0 inc}})
      (> [1 2 [3 4]] {[2 0] inc}))

  ;; index and keys mixed
  (is [1 2 [{:a 1} 4]]
      (> [1 2 [{:a 0} 4]]
         {[2 0 :a] inc}))

  ;; guarded transformation
  (is {:a {:b 2, :c -2}}
      (> {:a {:b 1 :c -1}}
         {:a {:b inc}
          [:a :c neg?] dec})) ;; this part will only update [:a :c] if it passes the neg? predicate

  ;; the same but failing (returning nil
  (isnt
    (> {:a {:b 1 :c 1}}
       {:a {:b inc}
        [:a :c neg?] dec}))

  ;; maybe you want to update [:a :c] only if its a negative number, and leave it unchanged if not
  (is {:a {:b 2, :c 1}}
      (> {:a {:b 1 :c 1}}
         {:a {:b inc}
          (l/? [:a :c neg?]) dec})) ;; for this you can use the ? lens


  ;; updating non existant key
  (is {:a {:b {:c 42}}}
      (> {}
         (at [:a :b :c] 42))
      (> {}
         {(l/path :a :b :c) 42}))

  ;; at can take several couples
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


  ;; the '> and <' have underscore and applied variations, please see utils-t 'testing-defn+

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

  (let [t (>_ {neg? inc}
              inc)]
    (is 1 (t -1))
    (isnt (t 1)))


  (isnt (> 1 (u/guard neg?) inc))

  (is (zero? (> -1 (u/guard neg?) inc)))

)