(ns speculoos.state)

(def ^:dynamic *cljs?* false)

(def state (atom {}))

(defn register-type! [n data]
  (swap! state assoc-in [:types n] data))

(defn registered-type? [x]
  (get-in @state [:types x]))