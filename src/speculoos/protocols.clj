(ns speculoos.protocols
  (:require [speculoos.utils :as u]
            [speculoos.state :as state]
            [speculoos.patterns :as ps]
            [speculoos.types :as st]))

(defmacro defproto
  [n & body]
  (u/expanding
    (let [[doc & body] (if (string? (first body)) body (cons nil body))
          proto-info
          (reduce (fn [ret [n & xs]]
                    (let [[return-spec sigs]
                          (if-not (vector? (first xs))
                            [(first xs) (next xs)] [nil xs])
                          arities (into {} (mapv (juxt count st/parse-fields) sigs))
                          sigs (map (partial mapv :sym) (vals arities))]
                      (assoc ret n {:arities arities :sigs sigs :return-spec return-spec})))
                  {} body)]
      (state/register-protocol! n proto-info)
      `(defprotocol ~n
         ~@(when doc [doc])
         ~@(mapv (fn [[n {:keys [sigs]}]]
                   (into () (reverse (list* n sigs)))) ;; this dummy thing is due to defrecord impl
                 proto-info)))))

(defmacro proto+ [p & body]
  (u/expanding
    (let [chunks
          (map (partial apply concat)
               (partition 2 (partition-by symbol? body)))

          proto-info
          (state/registered-protocol? p)

          return-spec
          (reduce (fn [ret [n {:keys [return-spec]}]]
                    (assoc ret n return-spec))
                  {} proto-info)

          format-method-body
          (fn [body]
            (cond (every? seq? body)
                  (map (fn [[argv & [_ & body+ :as body] :as all]]
                         (if-not body+ all (list argv (list* 'do body))))
                       body)

                  (every? vector? (take-nth 2 body))
                  (partition 2 body)

                  (vector? (first body))
                  (list (list (first body) (list* 'do (next body))))
                  :else (throw (Exception. (str "not valid method body " body)))))

          inject-argument-specs
          (fn [meth-name [argv body]]
            (let [parsed-arity (get-in proto-info [meth-name :arities (count argv)])
                  argv (mapv (fn [arg arg-info]
                               (if-let [spec (:spec arg-info)]
                                 (list arg :- spec)
                                 arg))
                             argv
                             parsed-arity)]
              (list argv body)))

          compile-method
          (fn [type [metname & body]]
            (let [body (format-method-body body)
                  injected-body
                  (mapcat (partial inject-argument-specs metname) body)
                  fm-form
                  (list* `ps/fm metname :- (return-spec metname) injected-body)
                  error-case
                  [:else
                   `(u/error "error in " '~metname "'s " '~type "'s implementation"
                             "\ncould not match any of those patterns:\n "
                             ~@(interpose "\n " (map #(list 'quote %) (take-nth 2 injected-body))))]]
              (-> fm-form
                  (concat error-case)
                  macroexpand-1
                  next)))

          formatted-chunks
          (mapcat (fn [[type & mets]]
                    (cons type (mapv (partial compile-method type) mets)))
                  chunks)]

      `(extend-protocol ~p
         ~@(do formatted-chunks)))))

(comment

  (state/registered-protocol? 'P5)

  (defproto P5
            (pouet string? [_ (b :- string?)]))

  (proto+ P5
          String
          (pouet [a b] (str a b)))

  (pouet "iop" 1)

  ((ps/fm iop :- string? [a (b :- string?)] (str a b)) "iop" "io")
  (macroexpand-1 (list* `ps/fm 'pouet :- 'string?
                        '([a (b :- string?)] (str a b)))))