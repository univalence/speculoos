(ns speculoos.types
  (:require [speculoos.utils :as u]
            [speculoos.specs :as ss]
            [speculoos.state :as state :refer [*cljs?*]]
            [speculoos.patterns :refer [defm] :as ps]
            [spec-tools.data-spec :as ds]
            [clojure.test.check.generators :as tcg]))

;; impl ------------------------------------------------------------------------------------

(defn optional-field? [{:keys [sym]}]
  (and (list? sym) (= '? (first sym))))

(defn flat-optional-fields [parsed-fields]
  (mapv (fn [x]
          (if (optional-field? x)
            (assoc x :optional true :sym (second (:sym x)))
            x))
        parsed-fields))

(defn fields-vec [fs]
  (cond
    (vector? fs) fs
    (map? fs) (map #(list (key %) :- (val %)) fs)
    :else (throw (Exception. (str "not a valid record field spec " fs)))))

(defn parse-fields [xs]
  (loop [ret [] seed (fields-vec xs)]
    (if-not (seq seed)
      (flat-optional-fields ret)
      (cond
        ;; unwrapped spec shorthand pattern
        (qualified-keyword? (second seed))
        (recur (conj ret {:sym (first seed) :spec (second seed)}) (drop 2 seed))

        ;; unwrapped spec pattern
        (= :- (second seed))
        (recur (conj ret {:sym (first seed) :spec (nth seed 2)}) (drop 3 seed))

        ;; wrapped spec pattern
        (ps/spec-pattern? (first seed))
        (recur (conj ret {:sym (ffirst seed) :spec (-> seed first (nth 2))}) (rest seed))

        ;; wrapped spec pattern shorthand
        (ps/spec-shorthand-pattern? (first seed))
        (recur (conj ret {:sym (ffirst seed) :spec (-> seed first second)}) (rest seed))

        :else
        (recur (conj ret {:sym (first seed)}) (rest seed))))))

(defn binding-form [parsed-fields]
  (->> parsed-fields
       (mapcat (fn [{:keys [spec sym optional]}]
                 (when spec
                   [sym (ss/conformer-strict-form
                          (if optional (list (ss/spec-sym "nilable") spec) spec) sym
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
        req-syms (mapv :sym (remove :optional parsed-fields))
        req-ks (map (comp keyword name) req-syms)
        opt-syms (mapv :sym (filter :optional parsed-fields))
        opt-ks (map (comp keyword name) opt-syms)]
    `(fn [{:keys ~syms :as ~s}]
       (let ~(binding-form parsed-fields)
         (~constr-sym
           (merge ~s
                  ~(zipmap req-ks req-syms)
                  (u/rem-nil-vals ~(zipmap opt-ks opt-syms))))))))

(defn unpositional-constructor-form [constr-sym]
  `(fn [& xs#] (~constr-sym (apply hash-map xs#))))

;; macros -------------------------------------------------------------------------------------

(defmacro deft
  [n fields & body]

  (binding [*cljs?* (and (:ns &env) true)]

    (let [;; fields
          positional? (vector? fields)
          parsed-fields (parse-fields fields)
          req-fields (remove :optional parsed-fields)
          opt-fields (filter :optional parsed-fields)
          fields-names (mapv :sym parsed-fields)
          req-fields-names (mapv :sym req-fields)

          ;; symbols
          record-sym (u/name->class-symbol n)
          map-constr-sym (u/mksym 'map-> n)
          map-builtin-constr-sym (u/mksym 'map-> record-sym)
          predsym (u/mksym n "?")
          pprint-sd-sym (if *cljs?* 'cljs.pprint/simple-dispatch 'clojure.pprint/simple-dispatch)

          ;; sub specs
          spec-keyword (keyword (str *ns*) (name n))
          sub-spec-form
          (fn [{:keys [sym spec]}]
            `(~(ss/spec-sym "def")
               ~(keyword (str *ns* "." n) (str sym))
               ~(or spec `any?)))
          sub-specs (map sub-spec-form req-fields)
          optional-sub-specs (map sub-spec-form opt-fields)]



      (state/register-type! n {:record-symbol record-sym
                               :map-constructor-symbol map-constr-sym
                               :predicate-symbol predsym
                               :spec-keyword spec-keyword
                               :fields fields-names})

      `(do (declare ~n ~predsym ~map-constr-sym ~map-builtin-constr-sym)

           ;; specs
           (~(ss/spec-sym "def") ~spec-keyword any?) ;; declare main spec for potential recursion
           ~@sub-specs ~@optional-sub-specs ;; fields specs
           (~(ss/spec-sym "def") ~spec-keyword
             (-> (ss/spec->SpecImpl (~(ss/spec-sym "keys")
                                      :req-un ~(mapv second sub-specs)
                                      :opt-un ~(mapv second optional-sub-specs)))
                 ;; wrapping the generator and the conformer
                 (update :gen
                         #(fn [& xs#]
                            (tcg/fmap ~map-builtin-constr-sym
                                      (apply % xs#))))
                 (update :conform
                         #(fn [s# x#]
                            (let [ret# (% s# x#)]
                              (if (~(ss/spec-sym "invalid?") ret#)
                                ret#
                                (~map-builtin-constr-sym ret#)))))))

           ;; record declaration
           (defrecord ~record-sym ~req-fields-names ~@body)

           ;; constructors
           ~(if positional?
              `(def ~n ~(positional-constructor-form (u/mksym '-> record-sym) parsed-fields))
              `(def ~n ~(unpositional-constructor-form map-constr-sym)))

           (def ~map-constr-sym ~(map-constructor-form map-builtin-constr-sym parsed-fields))

           ;; predicate
           (def ~predsym (fn [x#] (instance? ~record-sym x#)))

           ;; printing
           (defmethod ~pprint-sd-sym
             ~record-sym [x#]
             (~pprint-sd-sym (cons '~n (map (partial get x#) ~(mapv keyword fields-names)))))

           ~(if positional?

              (if *cljs?*

                `(extend-protocol cljs.core/IPrintWithWriter
                   ~record-sym
                   (cljs.core/-pr-writer [x# w# _#]
                     (let [extra-keys# (dissoc x# ~@(mapv keyword fields-names))]
                       (cljs.core/write-all
                         w# (if (seq extra-keys#)
                              (cons '~n (mapcat identity x#))
                              (cons '~n (map (partial get x#) ~(mapv keyword fields-names))))))))

                `(defmethod print-method
                   ~record-sym [x# w#]
                   (let [extra-keys# (dissoc x# ~@(mapv keyword fields-names))]
                     (print-method
                       (if (seq extra-keys#)
                         (cons '~n (mapcat identity x#) )
                         (cons '~n (map (partial get x#) ~(mapv keyword fields-names))))
                       w#))))

              (if *cljs?*

                `(extend-protocol cljs.core/IPrintWithWriter
                   ~record-sym
                   (cljs.core/-pr-writer [x# w# _#]
                     (cljs.core/write-all w# (cons '~n (mapcat identity x#)))))

                `(defmethod print-method
                   ~record-sym [x# w#]
                   (print-method (cons '~n (mapcat identity x#)) w#))))

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
