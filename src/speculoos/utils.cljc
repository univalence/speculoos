(ns speculoos.utils
  (:refer-clojure :exclude [#?(:cljs object?) empty declare])
  (:require
    [clojure.string :as str]
    [clojure.test]
    [cljs.test]
    [clojure.walk :refer [postwalk]]
    #?(:clj [speculoos.state :as state :refer [*cljs?*]])
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

     #_(defmacro print-cljs-ns []
         (clojure.pprint/pprint (:ns &env))
         nil)

     #_(defmacro resolve-cljs-symbol []
         (clojure.pprint/pprint (:ns &env))
         nil)

     (defn qualify-symbol [env sym]
       (if (:ns env)
         (:name (cljs.analyzer/resolve-var env sym))
         (if-let [v (resolve env sym)]
           (let [{:keys [ns name]} (meta v)]
             (symbol (str ns) (str name))))))

     (defmacro qualified-symbol [sym]
       (list 'quote (qualify-symbol &env sym)))

     (defmacro throws [expr]
       `(is ::catched
            (try ~expr
                 (catch ~(if (:ns &env)
                           'js/Error
                           'Exception)
                        ~'_
                   ::catched))))

     #_(defmacro expansion-error [expr]
         (is ::catched
             (try (clojure.walk/macroexpand-all expr)
                  (catch Exception _ ::catched))))))

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

;; ns

#?(:clj
   (do :namespaces

       (defn ->ns [x]
         (cond (symbol? x) (when (find-ns x) (the-ns x))
               (instance? clojure.lang.Namespace x) x))

       (defn ns-mappings
         ([] (ns-mappings *ns*))
         ([ns] #_(cond (symbol? ns) (when (find-ns ns) (.getMappings (the-ns ns)))
                       (instance? clojure.lang.Namespace ns) (.getMappings ns))
          (.getMappings (->ns ns))))

       (defn core-exclude [x]
         (if (coll? x)
           (doseq [x1 x]
             #_(println "excloop" x1)
             (core-exclude x1))
           (let [ms (ns-mappings)
                 found (get ms x)
                 from-ns (:ns (meta found))]
             (when (= (the-ns 'clojure.core) from-ns)
               #_(println "excluding " x " from " *ns*)
               (.unmap *ns* x)))))

       (defn core-mappings
         ([] (core-mappings *ns*))
         ([x] (->> (ns-mappings x)
                   (filter (fn [[k v]] (and (-> v meta :ns (= (the-ns 'clojure.core)))
                                            (-> v meta :private not))))
                   (into {})
                   keys set)))

       (defn core-exclusions
         ([] (core-exclusions *ns*))
         ([x]
          (clojure.set/difference
            (core-mappings 'clojure.core)
            (core-mappings x)
            #_(-> (ns-mappings x) keys set))))

       (comment
         ((core-exclusions) 'empty)
         ((core-mappings) 'empty)
         ((core-mappings 'clojure.core) 'empty))))



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
              (let [ss (dotsplit (name x))]
                (and (seq (next ss))
                     (every? #(not (re-matches #"^[A-Z].*" %)) ss)))))

       (defn dotsym->qualified-sym [x]
         (let [ss (dotsplit (name x))
               ns (namespace x)]
           (symbol (-> (if ns (cons ns ss) ss) butlast dotjoin name)
                   (last ss))))

       (defn walk-dotsyms [body]
         (state/if-cljs
           body
           (letfn [(walk [body]
                     (walk? body
                            ;; node?
                            (fn [x] (and (not (dof-form? x)) (coll? x)))
                            ;; leaf transform
                            (fn [x] (cond (dof-form? x) (concat (take 2 x) (walk (drop 2 x)))
                                          (dotsym? x) (dotsym->qualified-sym x)
                                          :else x))))]
             (walk body))))

       (defn doall-rec
         "realize all nested potetially nested lazy sequences
          usefull in macros, because when using dynamic vars based expansion state, we have to be sure that there is no lazyness in the expansion
          otherwise dynamic vars will not be bounded as intended when expansion lazy parts are realized"
         [x]
         (cond (seq? x) (or (seq x) ())
               (coll? x) ($ x doall-rec)
               :else x))

       (defmacro with-dotsyms [& body]
         (if (:ns &env)
           `(do ~@body) ;;in clojurescript we do nothing
           `(do ~@(walk-dotsyms body))))))



#?(:clj (do :defmac

            (defmacro expanding [& body]
              `(state/binding-expansion-dynamic-vars
                 (with-dotsyms
                   ~@(mapv (fn [x] `(doall-rec ~x)) body))))

            (defmacro defmac
              "a version of defmacro that binds expansion dynamic vars,
              force laziness realisation and handle dotsyms
              WIP"
              [& body]
              (let [{:keys [cases name]} (parse-fn body)]
                `(defmacro ~name
                   ~@(mapv
                       (fn [[argv & body]]
                         `(~argv
                            (state/binding-expansion-dynamic-vars
                              (let [~argv (walk-dotsyms ~argv)]
                                ~@(mapv (fn [e] `(doall-rec (with-dotsyms ~e))) body)))))
                       cases))))

            (comment
              (macroexpand '(speculoos.utils/walk-dotsyms x.y.z))
              (macroexpand (defmac yop ([x] (println state/*cljs?*) `(str.split ~(str x) #"\.")) ([x y] y)))
              (macroexpand '(yop aze.aze))
              (clojure.walk/macroexpand-all '(defmac yop ([x] x x.y.z) ([x y] y))))))

#?(:clj
   (do :dof

       ;; dof is a fancy version of def that let you define multi level vars
       ;; e.g
       ;; (dof foo {:a :map})
       ;; (dof foo.bar {:some :subvar})
       ;; foo => {:a :map}
       ;; foo.bar => {:some :subvar}
       ;; in clojure this dot notation is not legal since it is reserved to classes
       ;; but we can wrap some code containing dot notation in the 'with-dotsyms macro
       ;; this will walk replace all dot symbols replacing them by their corresponding qualified-symbol notation
       ;; foo.bar.baz will become foo.bar/baz and everything shoud word as intended
       ;; in the future we could make this invisible to the user by wrapping all top level forms... but it looks kind of heavy


       (def dof-state (atom {}))

       (defn dof-get-cljs-ns
         ([] (dof-get-cljs-ns (ns-symbol)))
         ([s] (get-in @dof-state [:cljs s])))

       (defn dof-get-clj-ns
         ([] (dof-get-clj-ns (ns-symbol)))
         ([x]
          (let [ns-sym
                (cond (instance? clojure.lang.Namespace x) (symbol (str x))
                      (string? x) (symbol x)
                      (symbol? x) x)]
            (get-in @dof-state [:clj ns-sym]))))

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

       (defn dof-cljs-primitive? [x]
         (or (string? x) (number? x)))

       (defn dof-cljs-primitive-check
         [[sym val] parents childs]
         (when (dof-cljs-primitive? val)
           (assert (not (seq childs))
                   (str "Can't bound a var that has childs to a js primitive (number or string)\n"
                        "var: " [sym val] "\nhas childs:\n" childs)))
         (mapv (fn [[n v]]
                 (assert (not (dof-cljs-primitive? v))
                         (str "dof cljs error: parent chain contains primitive: " [n v])))
               parents))

       (defn dof-cljs-form! [sym v]
         (let [parents (dof-cljs-parents-couples sym)
               childs (dof-cljs-childs-couples sym)
               _ (dof-cljs-primitive-check [sym v] parents childs)
               all (concat parents (list [sym v]) childs)
               sets (mapv (fn [[k v]] `(set! ~k ~v)) all)
               root? (get (dof-get-cljs-ns) (ffirst all))]
           (swap! dof-state update-in
                  [:cljs (ns-symbol)] merge
                  (into {} all))
           #_(println 'root? root?)
           (if root?
             (list* 'do sets)
             (list* 'do (cons `(def ~@(first all)) (next sets))))))

       (defn dof-clj-parse-require-arg [x]
         (cond
           (symbol? x) {:name x :form x}
           (vector? x) (merge (apply hash-map (next x)) {:name (first x) :form x})))

       (defn dof-clj-require-sub-nss [require-arg]
         (let [{:keys [as name form refer]} (dof-clj-parse-require-arg require-arg)
               refer-all? (= :all refer)]
           (cons form
                 (->> (dof-get-clj-ns name)
                      (mapcat (fn [suffix]
                                (for [s (heads (dotsplit suffix))]
                                  [(dotjoin name s) :as (dotjoin (or as (when-not refer-all? name)) s)])))
                      vecset))))

       ;; any harsh feelings about this ?

       (defonce original-require @#'clojure.core/require)

       (alter-var-root #'clojure.core/require
                       (fn [_]
                         (fn [& xs]
                           #_(prob 'rexpanded (mapcat dof-clj-require-sub-nss xs))
                           (apply original-require (mapcat dof-clj-require-sub-nss xs)))))

       (defonce original-use @#'clojure.core/use)

       (alter-var-root #'clojure.core/use
                       (fn [_]
                         (fn [& xs]

                           (let [names
                                 (->> xs
                                      (map dof-clj-parse-require-arg)
                                      (map :name))

                                 sub-required
                                 (mapcat #(-> [% :refer :all]
                                              dof-clj-parse-require-arg
                                              dof-clj-require-sub-nss
                                              next)
                                         names)]

                             (apply original-use xs)

                             (when (seq sub-required)
                               (apply original-require sub-required))))))

       (comment (:clj @dof-state)
                (dof-clj-require-sub-nss 'speculoos.utils 'spu)
                (dof foo.bar.baz.pop {:my :data})
                (require '[speculoos.utils :as spu])
                (use 'speculoos.utils))

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
               publics (when known-ns? (keys (ns-publics (the-ns sub-ns-sym))))
               exclusions (into (core-exclusions) (cons varsym publics))]

           (swap! dof-state update-in
                  [:clj ns-sym]
                  (fn [nss]
                    (loop [nss nss
                           [ns-prefix & others :as all] (map dotjoin (heads (dotsplit ns-prefix)))]
                      (if-not (seq all)
                        nss
                        (recur (if-not ((set nss) ns-prefix)
                                 (conj (or nss []) ns-prefix)
                                 nss)
                               others)))))
           `(do

              ;; initialize unexistant parent namespaces
              ~@(mapv (fn [ns']
                        `(do (~'ns ~ns')
                             (core-exclude '~exclusions)))
                      undeclared-sub-nss)

              ;; actual namespace
              (~'ns ~sub-ns-sym)
              (core-exclude '~exclusions)
              (use '~ns-sym)


              ~@(->> (dof-get-clj-ns ns-sym)
                     (mapcat (fn [suffix]
                               (for [s (heads (dotsplit suffix))]
                                 `(~'require '[~(dotjoin ns-str s) :as ~(dotjoin s)]))))
                     (remove (fn [[_ [_ [_ _ a]]]] (= a ns-prefix)))
                     vecset)
              ;; require parent ns aliases
              ~@(mapv
                  (fn [[alias ns]]
                    `(require '[~(symbol (str ns)) :as ~alias]))
                  (.getAliases *ns*))

              ;; if parent ns contains a var of the same name it need to be unmapped
              (when (ns-resolve '~ns-sym '~varsym)
                #_(println "unmapping " '~varsym "in" '~sub-ns-sym "because of " (ns-resolve '~ns-sym '~varsym))
                (ns-unmap '~sub-ns-sym '~varsym))

              (def ~varsym
                (with-dotsyms ~v))

              ;; return to original namespace
              (~'in-ns '~ns-sym)
              ~@(for [n (map inc (range (count splitted-prefix)))]
                  `(require '[~(dotjoin ns-str (take n splitted-prefix))
                              :as ~(dotjoin (take n splitted-prefix))])))))

       (defn dof-trivial-case? [sym]
         (not (or (namespace sym)
                  (next (dotsplit sym)))))

       (defmacro dof
         ([sym]
          (if (:ns &env)
            `(dof ~sym ~(or (get (dof-get-cljs-ns) sym) `(cljs.core/clj->js {})))
            `(do (core-exclude '~sym) (dof ~sym {}))))
         ([sym value]
          (cond
            (:ns &env) (dof-cljs-form! sym value)
            (dof-trivial-case? sym) `(do (core-exclude '~sym) (def ~sym ~value))
            :else (dof-clj-form! sym value))))

       (defmacro declare [& xs]
         `(do ~@(map (fn [n] `(dof ~n)) xs)))

       (defmacro dofn [name & body]
         (let [fn-body (if (string? (first body)) (next body) body)]
           `(dof ~name (fn ~@fn-body))))))

(comment :xp

         (do :analyse

             (let [h (fn me [x]
                       (condp #(%1 %2) x
                         symbol? [x]
                         coll? (mapcat me x)
                         []))]

               (def all-syms (comp set h)))

             (defn shadows
               "given a binding form as the one that fn use for its args
                it return a set of shadowed syms"
               [binding-form]
               (->> (destructure [binding-form '_])
                    (take-nth 2) set
                    (clojure.set/intersection (all-syms binding-form))))

             )

         (defn compile-module
           ;; api arity
           ([{:keys [name decls]}]
            (compile-module [name] decls []))
           ;; looping arity
           ([ctx [decl :as decls] ret]
            (cond

              (not (seq decls)) ret

              (re-matches #"^d[eo]f.*$" (name (first decl)))
              (let [verb (cond (= "def" (name (first decl))) `dof
                               (= "defn" (name (first decl))) `dofn
                               :else (first decl))
                    name (dotjoin ctx (second decl))]
                (compile-module
                  ctx (next decls)
                  (conj ret (list* verb name (drop 2 decl)))))

              (= 'module (first decl))
              (compile-module
                ctx (next decls)
                (into ret (compile-module (conj ctx (second decl)) (drop 2 decl) [])))

              :else
              (compile-module
                ctx (next decls)
                (conj ret decl)))))

         (compile-module
           {:name 'bob
            :decls '[(def a 1)
                     (defn bib 42)
                     (module foo
                             (def p 42)
                             (println 'iop))
                     (println "iop")]}))







