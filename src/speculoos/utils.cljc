(ns speculoos.utils
  (:require
    [clojure.string :as str]
    [clojure.walk :refer [postwalk]]
    #?(:clj [speculoos.state :refer [*cljs?*]])
    [#?(:cljs cljs.pprint :clj clojure.pprint) :as pp]))

(defn pp [& xs]
  (mapv pp/pprint xs))

(defn prob
  "print all its arguments and return the last"
  [& xs]
  (apply pp xs) (last xs))

(defn error [& xs]
  (throw (#?(:cljs js/Error
             :clj  Exception.)
           (apply str xs))))

#?(:clj (defn error-form [& xs]
          `(throw (new ~(if *cljs?* 'js/Error 'Exception) (~'str ~@xs)))))

(defn is [x & xs]
  (if-not xs
    (assert x "is nil!")
    (assert (apply = x xs)
            (apply str "not equal: " x " " (interpose " " xs)))))

(defn word? [x]
  (or (string? x)
      (symbol? x)
      (keyword? x)))

(defn mksym [& xs]
  (->> xs (map name) (apply str) symbol))

(defn map-vals [f m]
  (into {} (map (fn [[k v]] [k (f v)]) m)))

(defn map-keys [f m]
  (into {} (map (fn [[k v]] [(f k) v]) m)))

(defn map-h [f m]
  (into {} (map (fn [e] (f (key e) (val e))) m)))

(defn name->class-symbol [x]
  (mksym 'R_ x))

(defn predicate-symbol? [x]
  (and (symbol? x)
       (= \? (last (name x)))))

(defn parse-int [x]
  (cond
    (int? x) x
    (or (string? x) (keyword? x))
    (#?(:cljs js/parseInt
        :clj  Integer/parseInt) (name x))))

(defn guard [f]
  (fn [x & xs]
    (when (apply f x xs) x)))

#?(:clj
   (do
     (defmacro f1 [pat & body]
       `(fn [~pat] ~@body))

     (defmacro f_ [& body]
       `(fn [~'_] ~@body))

     (defmacro defn+
       "behave the same as defn but will also define applied and underscore variations"
       [name & body]
       (let [name* (mksym name '*)
             name_ (mksym name '_)
             name_* (mksym name '_*)]
         `(do (declare ~name* ~name_ ~name_*)
              (defn ~name ~@body)
              (def ~name* (partial apply ~name))
              (defn ~name_ [& xs#] #(~name* % xs#))
              (def ~name_* (partial apply ~name_)))))))







