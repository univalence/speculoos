(ns speculoos.utils
  (:require
    [clojure.string :as str]
    [clojure.test :as test]
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

#?(:clj
   (do
     (defmacro is [x & xs]
       `(do (test/is ~x)
            (test/is (~'= ~x ~@xs))))
     (defmacro isnt [x & xs]
       `(test/is (~'= nil ~x ~@xs)))))

#_(defn is [x & xs]
  (if-not xs
    (assert x "is nil!")
    (assert (apply = x xs)
            (apply str "not equal: " x " " (interpose " " xs)))))

#_(defn isnt [& xs]
  (assert (every? not xs)
          (apply str "truthy: " (interpose " " xs))))

(defn gat [xs i]
  (if (>= i 0)
    (cond
      (vector? xs) (get xs i)
      (seq? xs) (first (drop i xs)))
    (gat (reverse xs) (- (inc i)))))

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

(defn rem-nil-vals [m]
  (into {} (filter val m)))

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
  (fn
    ([x] (when (f x) x))
    ([x y] (when (f x y) x))
    ([x y z] (when (f x y z) x))
    ([x y z & others] (when (apply f x y z others) x))))

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

(defn parse-fn [[fst & nxt :as all]]

  (let [[name fst & nxt]
        (if (symbol? fst)
          (cons fst nxt)
          (concat [nil fst] nxt))

        [doc fst & nxt]
        (if (string? fst)
          (cons fst nxt)
          (concat [nil fst] nxt))

        [opts fst & nxt]
        (if (map? fst)
          (cons fst nxt)
          (concat [{} fst] nxt))

        impls
        (if (vector? fst)
          {fst (vec nxt)}
          (into {}
                (map
                  (fn [[args & body]]
                    [args (vec body)])
                  (cons fst nxt))))]

    (assoc opts
      :name name
      :doc doc
      :impls impls
      :cases (mapv (partial apply list*) impls))))

#?(:clj
   (defmacro marked-fn

     "marked function,
      define an anonymous form (like fn)
      a def form (like defn)
      and a predicate function (like fn?)"

     [name & [doc]]

     `(do

        (defn ~(mksym "->" name) [f#]
          (vary-meta f# assoc ~(keyword name) true))

        (defmacro ~name
          ([f#] (list '~(mksym (str *ns*) "/->" name) f#))
          ([x# & xs#]
           (let [parsed# (parse-fn (cons x# xs#))]
             `(with-meta
                (fn ~(or (:name parsed#) (gensym)) ~@(:cases parsed#))
                {~~(keyword name) true}))))

        (defn ~(mksym name "?") [x#]
          (when (-> x# meta ~(keyword name)) x#))



        (defmacro ~(mksym 'def name) [name'# & body#]
          `(def ~name'# (~'~name ~@body#))))))




