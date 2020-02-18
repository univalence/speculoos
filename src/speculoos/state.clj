(ns speculoos.state)

(def ^:dynamic *cljs?* nil)
(def ^:dynamic *expansion-env* nil)

(defmacro binding-cljs-flag [& body]
  `(binding [*cljs?* (or *cljs?* (boolean (:ns ~'&env)))]
     ~@body))

(defmacro binding-expansion-dynamic-vars [& body]
  `(binding [*expansion-env* (or *expansion-env* ~'&env)
             *cljs?* (or *cljs?* (boolean (:ns ~'&env)))]
     ~@body))

(defmacro if-cljs [then else]
  `(if *cljs?* ~then ~else))

(def state (atom {}))

(defn target-kw []
  (if *cljs?* :cljs :clj))

(defn register-type! [n data]
  (swap! state assoc-in [:types (target-kw) n] data))

(defn registered-type? [x]
  (get-in @state [:types (target-kw) x]))

(defn register-protocol! [n data]
  (swap! state assoc-in [:protocols (target-kw) n] data))

(defn registered-protocol? [x]
  #_(println "search proto " x " in " (get-in @state [:protocols (target-kw)]))
  (get-in @state [:protocols (target-kw) x]))