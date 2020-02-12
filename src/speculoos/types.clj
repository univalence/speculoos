(ns speculoos.types
  (:require [clojure.core :as c]
            [speculoos.utils :as u]
            [speculoos.specs :as ss]
            [speculoos.state :as state :refer [*cljs?*]]
            [speculoos.patterns :refer [defm] :as ps]
            [spec-tools.data-spec :as ds]
            [clojure.test.check.generators :as tcg]
            [clojure.string :as str]))

;; impl ------------------------------------------------------------------------------------

(defn optional-field? [{:keys [sym]}]
  (and (list? sym) (= '? (first sym))))

(defn sub-type-field? [{:keys [spec]}]
  (or (map? spec) (vector? spec)))

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
  (println "posconstrform" constr-sym)
  (let [syms (mapv :sym parsed-fields)]
    `(fn ~syms
       (let ~(binding-form parsed-fields)
         (~constr-sym #_~(if *cljs?* constr-sym (u/dotsym->qualified-sym constr-sym)) ~@syms)))))

(defn map-constructor-form [constr-sym parsed-fields]

  (println "mapconstrform" constr-sym)
  (let [s (gensym)
        syms (mapv :sym parsed-fields)
        req-syms (mapv :sym (remove :optional parsed-fields))
        req-ks (map (comp keyword name) req-syms)
        opt-syms (mapv :sym (filter :optional parsed-fields))
        opt-ks (map (comp keyword name) opt-syms)]
    `(fn [{:keys ~syms :as ~s}]
       (let ~(binding-form parsed-fields)
         (~constr-sym #_~(if *cljs?* constr-sym (u/dotsym->qualified-sym constr-sym))
           (merge ~s
                  ~(zipmap req-ks req-syms)
                  (u/rem-nil-vals ~(zipmap opt-ks opt-syms))))))))

(defn unpositional-constructor-form [constr-sym]
  (println "unposconstrform" constr-sym)
  `(fn [& xs#] (~constr-sym #_~(if *cljs?* constr-sym (u/dotsym->qualified-sym constr-sym)) (apply hash-map xs#))))

;; macros -------------------------------------------------------------------------------------

#_(defmacro deft
  [n fields & body]

  (binding [*cljs?* (boolean (:ns &env))]

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
          (fn [{:as field :keys [sym spec]}]
            (when-not (sub-type-field? field)
              `(~(ss/spec-sym "def")
                 ~(keyword (str *ns* "." n) (str sym))
                 ~(or spec `any?))))
          sub-specs (keep sub-spec-form req-fields)
          optional-sub-specs (keep sub-spec-form opt-fields)]



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
                         (cons '~n (mapcat identity x#))
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

(do :deft-new

    (defn parse-deft
      [[fullname fields-or-spec & body :as all]]
      #_(println 'parsing-deft all)

      (let [segs (u/dotsplit fullname)
            ns-str (str *ns*)
            ns (symbol ns-str)
            name-str (last segs)
            name (symbol name-str)
            prefix (when (next segs) (u/dotjoin (butlast segs)))
            prefix-str (when prefix (c/name prefix))
            spec-keyword (keyword (c/name (u/dotjoin ns prefix)) name-str)
            alias? (not (or (map? fields-or-spec) (vector? fields-or-spec)))
            fields (when-not alias? (parse-fields fields-or-spec))
            req-fields (remove :optional fields)
            opt-fields (filter :optional fields)
            req-fields-names (mapv :sym req-fields)
            opt-fields-names (mapv :sym opt-fields)
            spec (if alias? fields-or-spec)
            positional? (vector? fields-or-spec)
            record-sym (apply u/mksym 'R_ (interpose '_ segs))
            subs (when-not alias?
                   (mapv (fn [{:keys [spec sym optional]}]
                           (-> (parse-deft
                                 [(u/dotjoin prefix name sym)
                                  (or spec `any?)])
                               (assoc :optional optional)))
                         fields))
            subspecs
            (reduce (fn [a {:keys [name spec-keyword]}]
                      (assoc a name spec-keyword))
                    {} subs)

            req-fields-specs (mapv subspecs req-fields-names)
            opt-fields-specs (mapv subspecs opt-fields-names)]

        {;; names
         :ns ns :ns-str ns-str
         :fullname fullname
         :name name :name-str name-str
         :prefix prefix :prefix-str prefix-str
         :spec-keyword spec-keyword
         ;; function syms
         :predicate-sym (u/mksym fullname "?")
         :record-sym record-sym
         :builtin-map-constructor-sym (u/mksym 'map-> record-sym)
         :map-constructor-sym (u/dotjoin fullname 'from-map)
         ;; fields
         :fields fields :fields-names (mapv :sym fields)
         :req-fields req-fields :req-fields-names req-fields-names :req-fields-specs req-fields-specs
         :opt-fields opt-fields :opt-fields-names opt-fields-names :opt-fields-specs opt-fields-specs
         ;; misc
         :spec spec
         :positional positional? :alias alias?
         :body body :subs subs}))

    #_(clojure.pprint/pprint (parse-deft '[iop {a [b c] b number? c {d integer? e [f g]}}]))

    (defn emit-deft
      [{:as parsed
        :keys [ns ns-str name name-str
               fullname spec-keyword
               fields fields-names
               req-fields req-fields-names req-fields-specs
               opt-fields opt-fields-names opt-fields-specs
               body spec subs
               alias positional
               record-sym predicate-sym
               map-constructor-sym builtin-map-constructor-sym]}]



      (if spec

        `(u/with-dotsyms
           (~(ss/spec-sym "def") ~spec-keyword ~spec)
           (u/dof ~predicate-sym ~(ss/validator-form spec))
           (u/dof ~fullname ~(ss/conformer-form spec)))

        (let [pprint-sd-sym (if *cljs?* 'cljs.pprint/simple-dispatch 'clojure.pprint/simple-dispatch)]

          (state/register-type! name parsed)

          `(do (declare ~fullname ~predicate-sym ~builtin-map-constructor-sym)

               ;; specs
               (~(ss/spec-sym "def") ~spec-keyword any?) ;; declare main spec for potential recursion

               ~@(mapv emit-deft subs)

               ;~@sub-specs ~@optional-sub-specs ;; fields specs
               (~(ss/spec-sym "def") ~spec-keyword
                 (-> (ss/spec->SpecImpl (~(ss/spec-sym "keys")
                                          :req-un ~req-fields-specs
                                          :opt-un ~opt-fields-specs))
                     ;; wrapping the generator and the conformer
                     (update :gen
                             #(fn [& xs#]
                                (tcg/fmap ~builtin-map-constructor-sym
                                          (apply % xs#))))
                     (update :conform
                             #(fn [s# x#]
                                (let [ret# (% s# x#)]
                                  (if (~(ss/spec-sym "invalid?") ret#)
                                    ret#
                                    (~builtin-map-constructor-sym ret#)))))))

               ;; record declaration
               (defrecord ~record-sym ~req-fields-names ~@body)

               (u/dof ~map-constructor-sym ~(map-constructor-form builtin-map-constructor-sym fields))

               ;; constructors
               ~(if positional
                  `(u/dof ~fullname ~(positional-constructor-form (u/mksym '-> record-sym) fields))
                  `(u/dof ~fullname ~(unpositional-constructor-form
                                       (if *cljs?* map-constructor-sym (u/dotsym->qualified-sym map-constructor-sym)))))

               ;; predicate
               (u/dof ~predicate-sym (fn [x#] (instance? ~record-sym x#)))

               ;; printing
               (defmethod ~pprint-sd-sym
                 ~record-sym [x#]
                 (~pprint-sd-sym (cons '~name (map (partial get x#) ~(mapv keyword fields-names)))))

               ~(if positional

                  (if *cljs?*

                    `(extend-protocol cljs.core/IPrintWithWriter
                       ~record-sym
                       (cljs.core/-pr-writer [x# w# _#]
                         (let [extra-keys# (dissoc x# ~@(mapv keyword fields-names))]
                           (cljs.core/write-all
                             w# (if (seq extra-keys#)
                                  (cons '~name (mapcat identity x#))
                                  (cons '~name (map (partial get x#) ~(mapv keyword fields-names))))))))

                    `(defmethod print-method
                       ~record-sym [x# w#]
                       (let [extra-keys# (dissoc x# ~@(mapv keyword fields-names))]
                         (print-method
                           (if (seq extra-keys#)
                             (cons '~fullname (mapcat identity x#))
                             (cons '~fullname (map (partial get x#) ~(mapv keyword fields-names))))
                           w#))))

                  (if *cljs?*

                    `(extend-protocol cljs.core/IPrintWithWriter
                       ~record-sym
                       (cljs.core/-pr-writer [x# w# _#]
                         (cljs.core/write-all w# (cons '~fullname (mapcat identity x#)))))

                    `(defmethod print-method
                       ~record-sym [x# w#]
                       (print-method (cons '~fullname (mapcat identity x#)) w#))))

               ))))

    (defmacro deft
      [& body]
      (binding [*cljs?* (or *cljs?* (boolean (:ns &env)))]
        (-> body parse-deft emit-deft))))

(comment :new-deft
         (clojure.walk/macroexpand-all '(deft box [vl]))
         (deft box [val])
         (do box/val)
         (get (ns-map (the-ns 'speculoos.types.box)) 'val)
         (require '[speculoos.types.box :as box])
         (box/val 1)
         (box 1)

         (require '[clojure.spec.alpha :as s]) (s/conform ::box {:val 1}))

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
                                       (u/with-dotsyms (~(u/dotjoin name 'from-map) x#))
                                       (apply ~sym x#)))))
                        (take-nth 2 (next body)))))))))

(comment :ns-mess
         (ns iop.iop
           (:refer-clojure :exclude [popl]))

         (def a 1)
         (def val (fn [x] x))
         (ns-publics (the-ns 'iop.iop))
         (do a)

         (ns bop.bop))
