(ns speculoos.flow
  (:refer-clojure :exclude [> <])
  (:require [speculoos.utils :as u #?(:clj :refer :cljs :refer-macros) [is isnt f1 f_ defn+]]
            [speculoos.lenses :as l]))

;; links

(defn link [a b]
  #?(:clj  (clojure.lang.MapEntry. a b)
     :cljs (cljs.core/MapEntry. a b nil)))

(def link? map-entry?)

;; transformations

(defprotocol ITrans (->trans [x]))
(defprotocol ICombine (combine [x y]))

(defn trans? [x]
  (or (when (fn? x) x)
      (when (satisfies? ITrans x) (->trans x))))

(do :impl

    (declare trans step)

    (defn vec->trans [v]
      (let [ts (map trans v)]
        (fn [y] (reduce #(%2 %1) y ts))))

    (defn link->trans [e]
      (fn [x]
        (l/mut x
               (key e)
               #(step % (val e)))))

    (defn map->trans [m]
      (vec->trans (mapv link->trans m))))

(defn trans
  "given anything, return a function representing a transformation (a reduction step of '> )
   used in 'step when its first argument (the seed) does not implement ICombine"
  [x]
  (or (trans? x)
      (cond
        (nil? x) identity
        (map? x) (map->trans x)
        (link? x) (link->trans x)
        (vector? x) (vec->trans x)
        :else (constantly x))))

;; flow

(defn step [x y]
  (if (satisfies? ICombine x)
    (combine x y)
    ((trans y) x)))

(defn+ >
       "thread x thru given transformations (xs) shortcircuiting on first nil result"
       [x & xs]
       (loop [x x xs xs]
         (if (and x xs)
           (recur (step x (first xs)) (next xs))
           x)))

(defn+ <
       "try all given transformations over x until the first non nil result"
       [x & xs]
       (loop [xs xs]
         (when (seq xs)
           (or (step x (first xs))
               (recur (next xs))))))

(defn at
  "arity 2:
    the at function returns a trans(able)
    built upon 'lenses/path, 'path does not have to point to an existing value to succeed
   variadic artity
    takes several couples (flat), that will be executed in order (by the transformation it returns)"
  ([path f]
   (link (l/path path) f))
  ([x y & zs]
   (reduce (fn [f [p g]]
             (comp (link->trans (at p g)) f))
           (link->trans (at x y))
           (partition 2 zs))))



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
