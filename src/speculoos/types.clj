(ns speculoos.types
  (:require [speculoos.utils :as u]
            [speculoos.specs :as ss]
            [speculoos.state :as state :refer [*cljs?*]]
            [speculoos.patterns :refer [defm] :as ps]
            [spec-tools.data-spec :as ds]
            [clojure.test.check.generators :as tcg]))

;; impl ------------------------------------------------------------------------------------

(defn parse-fields [xs]
  (loop [ret [] seed xs]
    (if-not (seq seed)
      ret
      (cond
        ;; unwrapped validation shorthand pattern
        (qualified-keyword? (second seed))
        (recur (conj ret {:sym (first seed) :validation (second seed)}) (drop 2 seed))

        (ps/validation-pattern? (first seed))
        (recur (conj ret {:sym (ffirst seed) :validation (-> seed first (nth 2))}) (rest seed))

        (ps/coercion-pattern? (first seed))
        (recur (conj ret {:sym (ffirst seed) :coercion (-> seed first (nth 2))}) (rest seed))

        (ps/coercion-shorthand-pattern? (first seed))
        (recur (conj ret {:sym (-> seed first second) :coercion (ffirst seed)}) (rest seed))

        (ps/validation-shorthand-pattern? (first seed))
        (recur (conj ret {:sym (ffirst seed) :validation (-> seed first second)}) (rest seed))

        ;; unwrapped validation pattern
        (= :- (second seed))
        (recur (conj ret {:sym (first seed) :validation (nth seed 2)}) (drop 3 seed))

        ;; unwrapped coercion pattern
        (= :< (second seed))
        (recur (conj ret {:sym (first seed) :coercion (nth seed 2)}) (drop 3 seed))

        :else
        (recur (conj ret {:sym (first seed)}) (rest seed))))))

(defn binding-form [parsed-fields]
  (->> parsed-fields
       (mapcat (fn [{:keys [coercion sym validation]}]
                 (cond
                   coercion
                   [sym (ss/conformer-strict-form
                          coercion sym
                          (u/error-form `(~'pr-str ~sym) " cannot be coerced by " coercion))]
                   validation
                   [sym (ss/validator-strict-form
                          validation sym
                          (u/error-form "invalid field value: " `(~'pr-str ~sym) " is not a valid " validation))]
                   :else [])))
       vec))

(defn positional-constructor-form [constr-sym parsed-fields]
  (let [syms (mapv :sym parsed-fields)]
    `(fn ~syms
       (let ~(binding-form parsed-fields)
         (~constr-sym ~@syms)))))

(defn map-constructor-form [constr-sym parsed-fields]
  (let [s (gensym)
        syms (mapv :sym parsed-fields)
        ks (map (comp keyword name) syms)]
    `(fn [{:keys ~syms :as ~s}]
       (let ~(binding-form parsed-fields)
         (~constr-sym (merge ~s ~(zipmap ks syms)))))))

;; macros -------------------------------------------------------------------------------------

(defmacro deft
  [n fields & body]

  (binding [*cljs?* (and (:ns &env) true)]

    (let [recsym (u/name->class-symbol n)
          parsed-fields (parse-fields fields)
          fields-names (mapv :sym parsed-fields)
          constr (positional-constructor-form (u/mksym '-> recsym) parsed-fields)
          map-constr (map-constructor-form (u/mksym 'map-> recsym) parsed-fields)
          map-constr-sym (u/mksym 'map-> n)
          predsym (u/mksym n "?")
          pprint-sd-sym (if *cljs?* 'cljs.pprint/simple-dispatch 'clojure.pprint/simple-dispatch)
          spec-keyword (keyword (str *ns*) (name n))
          spec-structure
          (zipmap (map keyword fields-names)
                  (map #(or (:coercion %) (:validation %) `identity)
                       parsed-fields))]

      (state/register-type! n {:record-symbol recsym
                               :map-constructor-symbol map-constr-sym
                               :predicate-symbol predsym
                               :spec-keyword spec-keyword
                               :fields fields-names})

      `(do (declare ~n ~predsym)
           (defrecord ~recsym ~fields-names ~@body)
           (def ~n ~constr)
           (def ~map-constr-sym ~map-constr)
           (def ~predsym (fn [x#] (instance? ~recsym x#)))
           (defmethod ~pprint-sd-sym
             ~recsym [x#]
             (~pprint-sd-sym (cons '~n (map (partial get x#) ~(mapv keyword fields)))))

           ~(if *cljs?*

              `(extend-protocol cljs.core/IPrintWithWriter
                 ~recsym
                 (cljs.core/-pr-writer [x# w# _#]
                   (cljs.core/write-all w# (cons '~n (map (partial get x#) ~(mapv keyword fields-names))))))

              `(defmethod print-method
                 ~recsym [x# w#]
                 (print-method (cons '~n (map (partial get x#) ~(mapv keyword fields-names)))
                               w#)))

           #_(us/defspec ~n)
           (~(ss/spec-sym "def") ~spec-keyword

             (-> (ss/spec->SpecImpl (ds/spec ~spec-keyword ~spec-structure))
                 ;; wrapping the generator and the conformer
                 (update :gen
                         #(fn [& xs#]
                            (tcg/fmap ~map-constr-sym
                                      (apply % xs#))))
                 (update :conform
                         #(fn [s# x#]
                            (let [ret# (% s# x#)]
                              (if (~(ss/spec-sym "invalid?") ret#)
                                ret#
                                (~map-constr-sym ret#)))))))))))

(defmacro defc
  "another taste of deft, see tutorial"
  [name fields & body]
  (let [sym (gensym)]
    `(do
       (deft ~name ~fields)
       (let [~sym ~name]

         (defm ~name
               ~@(interleave
                   (take-nth 2 body)
                   (map (fn [x] (if (vector? x)
                                  `(~sym ~@x) ;; little optimisation for litteral vectors (most common usecase)
                                  `(let [x# ~x]
                                     (if (map? x#)
                                       (~(u/mksym 'map-> name) x#)
                                       (apply ~sym x#)))))
                        (take-nth 2 (next body)))))))))