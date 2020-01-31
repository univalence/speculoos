(ns speculoos.core
  (:refer-clojure :exclude [< > get])
  (:require #?(:clj [clojure.spec.alpha :as s] :cljs [cljs.spec.alpha :as s])
            #?(:clj [clojure.core.match] :cljs [cljs.core.match])
            #?(:clj [speculoos.types :as st])
            #?(:clj [speculoos.patterns :as sp])
            [speculoos.specs :as ss]
            [speculoos.flow :as flow]
            [speculoos.lenses :as lenses]
            [speculoos.utils :as u #?(:clj :refer :cljs :refer-macros) [import-defn+]]))

#?(:clj
   (do (defmacro deft [& xs] `(st/deft ~@xs))
       (defmacro defc [& xs] `(st/defc ~@xs))
       (defmacro defm [& xs] `(sp/defm ~@xs))
       (defmacro fm [& xs] `(sp/fm ~@xs))
       (defmacro defspec [& xs] `(ss/defspec ~@xs))
       (defmacro spec [& xs] `(ss/spec ~@xs))
       (defmacro cpred [& xs] `(ss/cpred ~@xs))))

(def at flow/at)
(def path lenses/path)
(def ? lenses/?)

(u/import-defn+ flow/>)
(u/import-defn+ flow/<)
(u/import-defn+ lenses/get)
(u/import-defn+ lenses/mut)
(u/import-defn+ lenses/mut<)
(u/import-defn+ lenses/put)
(u/import-defn+ lenses/pass)
