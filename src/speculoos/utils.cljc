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

     (defn ns-symbol []
       (symbol (str *ns*)))

     (defmacro is [x & xs]
       (if (:ns &env)
         `(do (cljs.test/is ~x)
              (cljs.test/is (~'= ~x ~@xs)))
         `(do (clojure.test/is ~x)
              (clojure.test/is (~'= ~x ~@xs)))))

     (defmacro isnt [x & xs]
       (if (:ns &env)
         `(cljs.test/is (~'= nil ~x ~@xs))
         `(clojure.test/is (~'= nil ~x ~@xs))))

     (defmacro print-cljs-ns []
       (clojure.pprint/pprint (:ns &env))
       nil)))

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

(defn dotsplit [x]
  (when (word? x)
    (str/split (name x) #"\.")))

(defn dotjoin
  ([xs]
   (symbol (str/join "." (map name xs))))
  ([x & xs]
   (dotjoin (remove nil? (flatten (cons x xs))))))

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

(defn map-vals [f m]
  (into {} (map (fn [[k v]] [k (f v)]) m)))

(defn map-keys [f m]
  (into {} (map (fn [[k v]] [(f k) v]) m)))

(defn map-h [f m]
  (into {} (map (fn [e] (f (key e) (val e))) m)))

(defn rem-nil-vals [m]
  (into {} (filter val m)))

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

;; macros

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
   (do :fn-macros

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
                (def ~name_* (partial apply ~name_)))))

       (defmacro import-defn+ [sym]
         (let [n (symbol (name sym))
               ns' (namespace sym)
               qualified-sym (fn [postfix] (symbol ns' (name (mksym n postfix))))]
           `(do (def ~n ~sym)
                (def ~(mksym n "_") ~(qualified-sym "_"))
                (def ~(mksym n "*") ~(qualified-sym "*"))
                (def ~(mksym n "_*") ~(qualified-sym "_*")))))

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
              `(def ~name'# (~'~name ~@body#)))))))

#?(:clj
   (do :dotsyms

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

       (defmacro with-dotsyms [& body]
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
             `(do ~@(walk body)))))))

#?(:clj
   (do :dof

       (def dof-state (atom {}))

       (defn dof-get-cljs-ns []
         (get-in @dof-state [:cljs (ns-symbol)]))

       (comment (clojure.pprint/pprint @dof-state))

       (defn dof-cljs-childs-couples [sym]
         (let [sym-segs (dotsplit sym)
               sym-segs-cnt (count sym-segs)]
           (->> (dof-get-cljs-ns)
                (filter (fn [[k v]]
                          (let [k-segs (dotsplit k)]
                            (and (> (count k-segs) sym-segs-cnt)
                                 (= sym-segs (take sym-segs-cnt k-segs))))))
                (sort-by #(-> % key name count)))))

       (defn dof-cljs-parents-couples [sym]
         (let [sym-segs (dotsplit sym)
               sym-heads (heads sym-segs)]
           (map (fn [s] [s (get (dof-get-cljs-ns) s `(cljs.core/clj->js {}))])
                (butlast (map dotjoin sym-heads)))))

       (defn dof-cljs-form! [sym v]
         (let [parents (dof-cljs-parents-couples sym)
               childs (dof-cljs-childs-couples sym)
               all (concat parents (list [sym v]) childs)
               sets (mapv (fn [[k v]] `(set! ~k ~v)) all)
               root? (get-in @dof-state [:cljs (ffirst all)])]
           (swap! dof-state update-in
                  [:cljs (ns-symbol)] merge
                  (into {} all))
           (if false #_root?
             (list* 'do sets)
             (list* 'do (cons `(def ~@(first all)) (next sets))))))

       (defn dof-clj-form!
         [sym v]
         (let [ss (dotsplit sym)
               ns-str (str *ns*)
               ns-sym (symbol ns-str)
               ns-prefix (or (some-> sym namespace symbol) (dotjoin (butlast ss)))
               sub-nss-syms (map #(dotjoin [ns-str %]) (map dotjoin (butlast (heads ss))))
               sub-ns-sym (last sub-nss-syms)
               varsym (symbol (last ss))
               splitted-prefix (dotsplit ns-prefix)
               undeclared-sub-nss (remove find-ns (butlast sub-nss-syms))
               known-ns? (find-ns sub-ns-sym)
               publics (when known-ns? (keys (ns-publics (the-ns sub-ns-sym))))]

           (swap! dof-state update
                  ns-str
                  (fn [nss]
                    (if-not ((set nss) ns-prefix)
                      (conj (or nss []) ns-prefix)
                      nss)))
           `(do
              ~@(mapv (fn [ns'] `(~'ns ~ns')) undeclared-sub-nss)
              (~'ns ~sub-ns-sym
                (:refer-clojure :exclude
                  ~(doall (cons varsym publics)))
                (:require [~ns-sym :refer :all])
                ~@(->> (get @dof-state ns-str)
                       (mapcat (fn [suffix]
                                 (for [s (heads (dotsplit suffix))]
                                   (list :require [(dotjoin ns-str s) :as (dotjoin s)]))))
                       (remove (fn [[_ [_ _ a]]] (= a ns-prefix)))
                       vecset))
              (def ~varsym (with-dotsyms ~v))
              (~'in-ns '~ns-sym)
              ~@(for [n (map inc (range (count splitted-prefix)))]
                  `(require '[~(dotjoin ns-str (take n splitted-prefix))
                              :as ~(dotjoin (take n splitted-prefix))])))))

       (defn dof-trivial-case? [sym]
         (not (or (namespace sym)
                  (next (dotsplit sym)))))

       (defmacro dof
         ;; TODO handle core redef warnings
         ([sym]
          (if (:ns &env)
            `(dof ~sym (cljs.core/clj->js {}))
            `(dof ~sym {}))
          #_(let [v (get (dof-get-cljs-ns) sym)]
              (if (:ns &env)
                `(dof ~sym ~(or v `(cljs.core/clj->js {})))
                `(dof ~sym ~(or v {})))))
         ([sym value]
          (cond
            (:ns &env) (dof-cljs-form! sym value)
            (dof-trivial-case? sym) `(def ~sym ~value)
            :else (dof-clj-form! sym value))))

       (defmacro declare [& xs]
         `(do ~@(map (fn [n] `(dof ~n)) xs)))

       (defmacro dofn [name & body]
         (let [fn-body (if (string? (first body)) (next body) body)]
           `(dof ~name (fn ~@fn-body))))
       ))








