(ns speculoos.core
  (:require #?(:clj  [clojure.spec.alpha :as s] :cljs [cljs.spec.alpha :as s])
            #?(:clj  [clojure.core.match] :cljs [cljs.core.match])
            #?(:clj [speculoos.types :as st])
            #?(:clj [speculoos.patterns :as sp])
            [speculoos.specs :as ss]))

#?(:clj
   (do (defmacro deft [& xs] `(st/deft ~@xs))
       (defmacro defc [& xs] `(st/defc ~@xs))
       (defmacro defm [& xs] `(sp/defm ~@xs))
       (defmacro fm [& xs] `(sp/fm ~@xs))
       (defmacro defspec [& xs] `(ss/defspec ~@xs))
       (defmacro spec [& xs] `(ss/spec ~@xs))
       (defmacro cpred [& xs] `(ss/cpred ~@xs))))
