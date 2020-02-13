(ns speculoos.utils
  (:refer-clojure :exclude [#?(:cljs object?) empty declare])
  (:require
    [clojure.string :as str]
    [clojure.test]
    [cljs.test]
    [clojure.walk :refer [postwalk]]
    #?(:clj [speculoos.state :refer [*cljs?*]])
    [#?(:clj clojure.core :cljs cljs.core) :as c]
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

#?(:clj
   (do
     (defn error-form [& xs]
       `(throw (new ~(if *cljs?* 'js/Error 'Exception) (~'str ~@xs))))

     (defmacro is [x & xs]
       (if (:ns &env)
         `(do (cljs.test/is ~x)
              (cljs.test/is (~'= ~x ~@xs)))
         `(do (clojure.test/is ~x)
              (clojure.test/is (~'= ~x ~@xs)))))

     (defmacro isnt [x & xs]
       (if (:ns &env)
         `(cljs.test/is (~'= nil ~x ~@xs))
         `(clojure.test/is (~'= nil ~x ~@xs))))))

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

;; collections

(defn empty [x]
  (condp #(%1 %2) x
    record? (apply dissoc x (keys x))
    map-entry? []
    (c/empty x)))

(defn $fn [ffn]
  (fn [x f]
    (if (seq? x)
      (ffn f x)
      (into (empty x) (ffn f x)))))

(def shrink+ ($fn filter))
(def shrink- ($fn remove))
(def $! ($fn keep))
(def $ ($fn map))

(defn $vals [x f]
  ($ x (fn [[k v]] [k (f v)])))

(defn $keys [x f]
  ($ x (fn [[k v]] [(f k) v])))

(defn walk? [x node? f]
  (if (node? x)
    ($ x #(walk? % node? f))
    (f x)))

;; macros

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
         `(do (c/declare ~name* ~name_ ~name_*)
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

#?(:clj
   (defmacro import-defn+ [sym]
     (let [n (symbol (name sym))
           ns' (namespace sym)
           qualified-sym (fn [postfix] (symbol ns' (name (mksym n postfix))))]
       `(do (def ~n ~sym)
            (def ~(mksym n "_") ~(qualified-sym "_"))
            (def ~(mksym n "*") ~(qualified-sym "*"))
            (def ~(mksym n "_*") ~(qualified-sym "_*"))))))

(defn dotsplit [x]
  (when (word? x)
    (str/split (name x) #"\.")))

(defn dotjoin
  ([xs]
   (symbol (str/join "." (map name xs))))
  ([x & xs]
   (dotjoin (remove nil? (flatten (cons x xs))))))

(defn dof-form? [x]
  (and (seq? x)
       (symbol? (first x))
       (= "dof" (name (first x)))))

(defn dotsym? [x]
  (and (symbol? x)
       (let [ss (str/split (name x) #"\.")]
         (and (seq (next ss))
              (every? #(not (re-matches #"^[A-Z].*" %)) ss)))))

(defn dotsym->qualified-sym [x]
  (let [ss (str/split (name x) #"\.")]
    (symbol (str/join "." (butlast ss)) (last ss))))

#?(:clj (defmacro with-dotsyms [& body]
          (if (:ns &env)
            `(do ~@body) ;;in clojurescript we do nothing
            (letfn [(walk [body]
                      (walk? body
                             ;; node?
                             (fn [x] (and (not (dof-form? x)) (coll? x)))
                             ;; leaf transform
                             (fn [x] (cond (dof-form? x) (concat (take 2 x) (walk (drop 2 x)))
                                           (dotsym? x) (dotsym->qualified-sym x)
                                           :else x))))]
              `(do ~@(walk body))))))

(defonce dof-state (atom {}))

(defn heads [x]
  (for [n (range (count x))]
    (take (inc n) x)))

(defn vecset [xs]
  (:ret (reduce (fn [{:keys [seen ret]} x]
                  (if (seen x)
                    {:seen seen :ret ret}
                    {:seen (conj seen x)
                     :ret (conj ret x)}))
                {:seen #{} :ret []}
                xs)))

#_(is (vecset [1 2 3 2 1 4 5 4 6 3 7])
    [1 2 3 4 5 6 7])

#?(:clj
   (defmacro dof
     ;; TODO handle core redef warnings
     ([n]
      `(dof ~n ~(if (:ns &env) `(cljs.core/clj->js {}) {})))
     ([n v]
      (let [ss (dotsplit n)
            ns-str (str *ns*)
            ns-sym (symbol ns-str)
            cljs? (:ns &env)]
        (if-not (or (namespace n) (next ss))
          `(def ~n ~v) ;; trivial case
          (if cljs?
            (loop [ss ss ctx [] ret [`(cljs.core/declare ~(symbol (first ss)))]]
              (if-not (seq ss)
                (identity ;prob 'dof-cljs
                  (list* 'do ret))
                (let [head (first ss)
                      head-sym (symbol head)
                      ctx (conj ctx head)
                      varsym (dotjoin ctx)]
                  (recur (next ss) ctx
                         (conj ret `(set! ~varsym ~(if-not (next ss) v `(or ~varsym (cljs.core/clj->js {})))))))))

            ;; clojure
            (let [ns-prefix (or (some-> n namespace symbol) (dotjoin (butlast ss)))
                  sub-ns-sym (dotjoin [ns-str ns-prefix])
                  varsym (symbol (last ss))
                  splitted-prefix (dotsplit ns-prefix)]

              (swap! dof-state update
                     ns-str
                     (fn [nss]
                       (if-not ((set nss) ns-prefix)
                         (conj (or nss []) ns-prefix)
                         nss)))

              `(do
                 (~'ns ~sub-ns-sym
                   (:refer-clojure :exclude ~(doall (cons varsym (when (find-ns sub-ns-sym) (keys (ns-publics (the-ns sub-ns-sym)))))))
                   (:require [~ns-sym :refer :all])
                   ~@(->> (get @dof-state ns-str)
                          (mapcat (fn [suffix]
                                    #_(println (dotsplit suffix))
                                    (for [s (heads (dotsplit suffix))]
                                      (list :require [(dotjoin ns-str s) :as (dotjoin s)]))))
                          (remove (fn [[_ [_ _ a]]] (= a ns-prefix)))
                          vecset))
                 (def ~varsym (with-dotsyms ~v))
                 (~'in-ns '~ns-sym)
                 #_`(require '[~sub-ns-sym :as ~(symbol ns-prefix)])
                 ~@(for [n (map inc (range (count splitted-prefix)))]
                     `(require '[~(dotjoin ns-str (take n splitted-prefix)) :as ~(dotjoin (take n splitted-prefix))]))))))))))

(macroexpand '(dof a.b.c.d.e 1))
(macroexpand '(dof a.b.c 1))

#?(:cljs (defn object? [x]
           (= ::object x)))

#_(clojure.walk/macroexpand-all '(with-dotsyms
                                   (+ a m.k.l)
                                   clojure.lang.String
                                   (def r {p.l poi.mlk})
                                   (dof r.l.p {p.l poi.mlk})))

#_(macroexpand '(dof a.b.c 12))

#_(dof a.b.c 12)

#_(do *ns*)

#?(:clj (defmacro dofn [name & body]
          (let [fn-body (if (string? (first body)) (next body) body)]
            `(dof ~name (fn ~@body)))))

#?(:clj (defmacro defr
          "thin wrapper around defrecord that simply defines a predicate, in order to not have to import the class"
          [n & body]
          `(do (defrecord ~n ~@body)
               (defn ~(mksym n "?") [x#] (instance? ~n x#)))))

#?(:clj (defmacro declare [& xs]
          `(do ~@(map (fn [n] `(dof ~n)) xs))))

#?(:clj (defmacro print-cljs-ns []
          (clojure.pprint/pprint (:ns &env))
          nil))






