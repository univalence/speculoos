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

(defn binding-form
  [{:keys [subs]}]
  (->> subs
       (mapcat (fn [{:keys [spec-keyword spec name optional]}]
                 (when-not (= any? spec)
                   [name (ss/conformer-strict-form
                           (if optional (list (ss/spec-sym "nilable") spec-keyword) spec-keyword)
                           name
                           (u/error-form `(~'pr-str ~name) " cannot be conformed to " spec-keyword))])))
       vec))

(defn positional-constructor-form
  [{:as deft-spec
    :keys [builtin-positional-constructor-sym
           fields-names arity fullname]}]
  (let [fn-sym (gensym)
        applied-arg-sym (gensym)]
    `(fn ~fn-sym
       ~@(when (> arity 1)
           [`([~applied-arg-sym]
              (if (and (vector? ~applied-arg-sym)
                       (= ~arity (count ~applied-arg-sym)))
                (apply ~fn-sym ~applied-arg-sym)
                ~(u/error-form "not applicable to " (str fullname) "\n" applied-arg-sym)))])
       (~fields-names
         (let ~(binding-form deft-spec)
           (~builtin-positional-constructor-sym ~@fields-names))))))

(defn map-constructor-form
  [{:as deft-spec
    :keys [builtin-map-constructor-sym
           fields-names
           opt-fields-names
           req-fields-names]}]
  (let [s (gensym)
        req-ks (map (comp keyword name) req-fields-names)
        opt-ks (map (comp keyword name) opt-fields-names)]
    `(fn [{:keys ~fields-names :as ~s}]
       (let ~(binding-form deft-spec)
         (~builtin-map-constructor-sym
           (merge ~s
                  ~(zipmap req-ks req-fields-names)
                  (u/rem-nil-vals ~(zipmap opt-ks opt-fields-names))))))))

(defn unpositional-constructor-form
  [{:keys [map-constructor-sym]}]
  (let [constr (if *cljs?* map-constructor-sym (u/dotsym->qualified-sym map-constructor-sym))]
    `(fn
       ([x#] (~constr x#))
       ([x# & xs#] (~constr (apply hash-map x# xs#))))))

;; macros -------------------------------------------------------------------------------------

(do :deft

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
                               (assoc :optional optional
                                      :parent-spec spec-keyword)))
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
         :builtin-positional-constructor-sym (u/mksym '-> record-sym)
         :builtin-map-constructor-sym (u/mksym 'map-> record-sym)
         :map-constructor-sym (u/dotjoin fullname 'from-map)
         ;; fields
         :fields fields :fields-names (mapv :sym fields)
         :req-fields req-fields :req-fields-names req-fields-names :req-fields-specs req-fields-specs
         :opt-fields opt-fields :opt-fields-names opt-fields-names :opt-fields-specs opt-fields-specs
         ;; misc
         :spec spec
         :positional positional? :alias alias?
         :arity (and positional? (count fields))
         :body body :subs subs}))

    #_(clojure.pprint/pprint (parse-deft '[iop {a [b c] b number? c {d integer? e [f g]}}]))
    #_(parse-deft '(t4 {a [b c]
                        b {c integer?
                           d string?
                           e [f :- number?]}}))

    (parse-deft '(t4 {a {b [c :- number?]}}))
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
               map-constructor-sym builtin-map-constructor-sym
               builtin-positional-constructor-sym]}]

      ;; TODO, hide core redefs warnings
      (if spec

        `(u/with-dotsyms
           (~(ss/spec-sym "def") ~spec-keyword ~spec)
           (u/dof ~predicate-sym ~(ss/validator-form spec))
           (u/dof ~fullname ~(ss/conformer-form spec)))

        (let [pprint-sd-sym (if *cljs?* 'cljs.pprint/simple-dispatch 'clojure.pprint/simple-dispatch)]

          (state/register-type! [ns fullname] parsed)

          `(do
             (u/declare ~fullname ~predicate-sym)

             ;; record declaration

             (defrecord ~record-sym ~req-fields-names ~@body)
             (defn ~(u/mksym record-sym "?") [x#] (instance? ~record-sym x#))

             ;; specs
             (~(ss/spec-sym "def") ~spec-keyword any?) ;; declare main spec for potential recursion

             ~@(mapv emit-deft subs)

             (~(ss/spec-sym "def") ~spec-keyword
               (-> (ss/spec->SpecImpl (~(ss/spec-sym "keys")
                                        :req-un ~req-fields-specs
                                        :opt-un ~opt-fields-specs))
                   ;; wrapping the generator and the conformer
                   (update :gen
                           #(fn [& xs#]
                              (tcg/fmap ~builtin-map-constructor-sym
                                        (apply % xs#))))
                   #_(update :conform
                           #(fn [s# x#]
                              (let [ret# (% s# x#)]
                                (if (~(ss/spec-sym "invalid?") ret#)
                                  ret#
                                  (~builtin-map-constructor-sym ret#)))))

                   (update :conform
                           #(fn [s# ~'X] ;; gensyms are not unified thru nested syntax-quotes
                              (let [~'X ;; here we are taking care of conforming a vector to a positional type
                                    ~(if positional
                                       `(if (vector? ~'X)
                                          ((u/with-dotsyms ~fullname) ~'X)
                                          ~'X)
                                       'X)
                                    ret# (% s# ~'X)]
                                (if (~(ss/spec-sym "invalid?") ret#)
                                  ret#
                                  (~builtin-map-constructor-sym ret#)))))))



             (u/dof ~map-constructor-sym ~(map-constructor-form parsed))

             ;; constructors
             ~(if positional
                `(u/dof ~fullname ~(positional-constructor-form parsed))
                `(u/dof ~fullname ~(unpositional-constructor-form parsed)))

             ;; predicate
             (u/dof ~predicate-sym (fn [x#] (~(u/mksym record-sym "?") x#)))

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
                                (cons '~fullname (mapcat identity x#))
                                (cons '~fullname (map (partial get x#) ~(mapv keyword fields-names))))))))

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
        (-> body parse-deft emit-deft)))

    (defmacro defc
      "another taste of deft, see tutorial"
      [name fields & body]

      (let [sym (gensym)
            sym2 (gensym)

            map-constructor-sym
            (if (:ns &env)
              (u/dotjoin sym 'from-map)
              (symbol (c/name name) "from-map"))

            compile-return
            (fn [x]
              (if (vector? x)
                `(~sym ~@x) ;; little optimisation for litteral vectors (most common usecase)
                `(let [~sym2 ~x]
                   (if (map? ~sym2)
                     (~map-constructor-sym ~sym2)
                     (apply ~sym ~sym2)))))]

        `(do
           (deft ~name ~fields)
           (let [~sym ~name]
             (defm ~name
                   ~@(->> (take-nth 2 (next body))
                          (mapv compile-return)
                          (interleave (take-nth 2 body))
                          doall)))))))

(comment :deft-scratch
         (clojure.walk/macroexpand-all '(deft box [vl]))
         (deft box [val])
         (do box/val)
         (get (ns-map (the-ns 'speculoos.types.box)) 'val)
         (require '[speculoos.types.box :as box])
         (box/val 1)
         (box 1)

         (macroexpand '(deft t4 {a [b c]
                                 b {c integer?
                                    d string?
                                    e [f :- number?]}}))

         (deft t4 {a [b c]
                   b {c integer?
                      d string?
                      e [f :- number?]}})

         (t4 :a [1 2])

         (macroexpand '(deft t4 {a {b [c :- number?]}}))

         (do t4/a)
         (deft t4 {a {b [c :- number?]}})
         (macroexpand '(deft t4 {a {b number?}}))

         (deft t4 {a {b number?}})

         (t4 :a {:b "aze"})

         (require '[clojure.spec.alpha :as s]) (s/conform ::box {:val 1}))

(comment :recursive-spec-test

         (deft2 rec [a :- integer? b :- (s/nilable ::rec)])

         (macroexpand '(deft2 rec [a :- integer? b :- (s/nilable ::rec)]))

         (require '[clojure.spec.alpha :as s]
                  '[clojure.spec.gen.alpha :as gen])

         (s/valid? ::rec (rec 1 (rec 2 nil)))
         (gen/generate (s/gen ::rec)))



(comment :ns-mess
         (ns iop.iop.iop
           (:refer-clojure :exclude [val]))

         (def a 1)
         (def val (fn [x] x))
         (ns-publics (the-ns 'iop.iop))
         (do a)

         (ns a.b.c
           (:require [iop.iop :refer :all]
                     [iop.iop.iop :refer :all]))

         (do val)

         (ns bop.bop)

         ())
