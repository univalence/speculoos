(ns speculoos.state)

(def state (atom {}))

(def ^:dynamic *cljs?* nil)
(def ^:dynamic *expansion-env* nil)

(defmacro binding-expansion-dynamic-vars [& body]
  `(binding [*expansion-env* (or *expansion-env* ~'&env)
             *cljs?* (or *cljs?* (boolean (:ns ~'&env)))]
     ~@body))

(defmacro if-cljs [then else]
  `(if *cljs?* ~then ~else))

(defn qualify-symbol [x]
  (when (symbol? x)
    (if-cljs
      (:name (cljs.analyzer/resolve-var *expansion-env* x))
      (if-let [v (resolve *expansion-env* x)]
        (let [{:keys [ns name]} (meta v)]
          (symbol (str ns) (str name)))))))

(defn target-kw []
  (if-cljs :cljs :clj))

(defn register-type! [type-symbol data]
  (swap! state assoc-in [:types (target-kw) type-symbol] data))

(defn registered-type? [type-symbol]
  (get-in @state [:types (target-kw) type-symbol]))

(defn register-protocol! [n data]
  (assert (not (namespace n))
          "can only register a protocol for the current namespace")
  (swap! state assoc-in [:protocols (target-kw) (symbol (str *ns*) (name n))] data))

(defn registered-protocol? [x]
  #_(println "search proto " x " in " (get-in @state [:protocols (target-kw)]))
  (get-in @state [:protocols (target-kw) (qualify-symbol x)]))