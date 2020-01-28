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
        ;; unwrapped spec shorthand pattern
        (qualified-keyword? (second seed))
        (recur (conj ret {:sym (first seed) :spec (second seed)}) (drop 2 seed))

        ;; wrapper spec pattern
        (ps/spec-pattern? (first seed))
        (recur (conj ret {:sym (ffirst seed) :spec (-> seed first (nth 2))}) (rest seed))

        ;; wrapper spec pattern
        (ps/spec-shorthand-pattern? (first seed))
        (recur (conj ret {:sym (ffirst seed) :spec (-> seed first second)}) (rest seed))

        ;; unwrapped spec pattern
        (= :- (second seed))
        (recur (conj ret {:sym (first seed) :spec (nth seed 2)}) (drop 3 seed))

        :else
        (recur (conj ret {:sym (first seed)}) (rest seed))))))

(defn binding-form [parsed-fields]
  (->> parsed-fields
       (mapcat (fn [{:keys [spec sym]}]
                 (when spec
                   [sym (ss/conformer-strict-form
                          spec sym
                          (u/error-form `(~'pr-str ~sym) " cannot be conformed to " spec))])))
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

(defn unpositional-constructor-form [constr-sym parsed-fields]
  (let [s (gensym)
        syms (mapv :sym parsed-fields)
        ks (map (comp keyword name) syms)]
    `(fn self#
       ([{:keys ~syms :as ~s}]
        (let ~(binding-form parsed-fields)
          (~constr-sym (merge ~s ~(zipmap ks syms)))))
       ([x# & xs#] (self# (apply hash-map x# xs#))))))

;; macros -------------------------------------------------------------------------------------

(defmacro deft-v1
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

      `(do (declare ~n ~predsym ~map-constr-sym)

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
                                (~map-constr-sym ret#)))))))

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
           ))))

(defmacro deft
  [n fields & body]

  (binding [*cljs?* (and (:ns &env) true)]

    (let [record-sym (u/name->class-symbol n)
          positional? (vector? fields)
          fields (if positional? fields (map #(list (key %) :- (val %)) fields))
          parsed-fields (parse-fields fields)
          fields-names (mapv :sym parsed-fields)
          map-constr-sym (u/mksym 'map-> n)
          predsym (u/mksym n "?")
          pprint-sd-sym (if *cljs?* 'cljs.pprint/simple-dispatch 'clojure.pprint/simple-dispatch)
          spec-keyword (keyword (str *ns*) (name n))
          sub-specs
          (map (fn [{:keys [sym validation coercion]}]
                 `(~(ss/spec-sym "def")
                    ~(keyword (str *ns* "." n) (str sym))
                    ~(or coercion validation `any?)))
               parsed-fields)]



      (state/register-type! n {:record-symbol record-sym
                               :map-constructor-symbol map-constr-sym
                               :predicate-symbol predsym
                               :spec-keyword spec-keyword
                               :fields fields-names})

      `(do (declare ~n ~predsym ~map-constr-sym)

           ;; specs
           (~(ss/spec-sym "def") ~spec-keyword any?) ;; declare main spec for potential recursion
           ~@sub-specs ;; fields specs
           (~(ss/spec-sym "def") ~spec-keyword
             (-> (ss/spec->SpecImpl (~(ss/spec-sym "keys") :req-un ~(mapv second sub-specs)))
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
                                (~map-constr-sym ret#)))))))

           ;; record declaration
           (defrecord ~record-sym ~fields-names ~@body)

           ;; constructors
           ~(if positional?
              `(do
                 (def ~n ~(positional-constructor-form (u/mksym '-> record-sym) parsed-fields))
                 (def ~map-constr-sym ~(map-constructor-form (u/mksym 'map-> record-sym) parsed-fields)))
              `(def ~n ~(unpositional-constructor-form (u/mksym 'map-> record-sym) parsed-fields)))

           ;; predicate
           (def ~predsym (fn [x#] (instance? ~record-sym x#)))

           ;; printing
           (defmethod ~pprint-sd-sym
             ~record-sym [x#]
             (~pprint-sd-sym (cons '~n (map (partial get x#) ~(mapv keyword fields)))))

           ~(if positional?

              (if *cljs?*

                `(extend-protocol cljs.core/IPrintWithWriter
                   ~record-sym
                   (cljs.core/-pr-writer [x# w# _#]
                     (cljs.core/write-all w# (cons '~n (map (partial get x#) ~(mapv keyword fields-names))))))

                `(defmethod print-method
                   ~record-sym [x# w#]
                   (print-method (cons '~n (map (partial get x#) ~(mapv keyword fields-names))) w#)))

              (if *cljs?*

                `(extend-protocol cljs.core/IPrintWithWriter
                   ~record-sym
                   (cljs.core/-pr-writer [x# w# _#]
                     (cljs.core/write-all w# (cons '~n (mapcat #(list % (get x# %)) ~(mapv keyword fields-names))))))

                `(defmethod print-method
                   ~record-sym [x# w#]
                   (print-method (cons '~n (mapcat #(list % (get x# %)) ~(mapv keyword fields-names))) w#))))

           ))))

(comment :recursive-spec-test

         (deft2 rec [a :- integer? b :- (s/nilable ::rec)])

         (macroexpand '(deft2 rec [a :- integer? b :- (s/nilable ::rec)]))

         (require '[clojure.spec.alpha :as s]
                  '[clojure.spec.gen.alpha :as gen])

         (s/valid? ::rec (rec 1 (rec 2 nil)))
         (gen/generate (s/gen ::rec)))

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
