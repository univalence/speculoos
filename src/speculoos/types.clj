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

#_(defn binding-form
    [{:keys [fields parent-spec]}]
    (->> fields
         (mapcat (fn [{:keys [spec sym optional]}]
                   (println spec)
                   (when spec
                     [sym (ss/conformer-strict-form
                            (if optional (list (ss/spec-sym "nilable") spec) spec) sym
                            (u/error-form `(~'pr-str ~sym) " cannot be conformed to " spec))])))
         vec))

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
           fields-names]}]
  `(fn ~fields-names
     (let ~(binding-form deft-spec)
       (~builtin-positional-constructor-sym ~@fields-names))))

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
    `(fn [& xs#] (~constr (apply hash-map xs#)))))

;; macros -------------------------------------------------------------------------------------

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
         :body body :subs subs}))

    #_(clojure.pprint/pprint (parse-deft '[iop {a [b c] b number? c {d integer? e [f g]}}]))
    #_(parse-deft '(t4 {a [b c]
                      b {c integer?
                         d string?
                         e [f :- number?]}}))

    (parse-deft '(t4 {a {b [c :- number?]}}))
    (defn emit-deft
      [{:as parsed
        ns' :ns
        :keys [ns-str name name-str
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

          (state/register-type! name parsed)

          `(do ;(ns-unmap '~ns '~fullname)
             ;(ns-unmap '~ns '~predicate-sym)
             ;~(when-not *cljs?* `(ns ~ns' (:refer-clojure :exclude [~fullname ~predicate-sym])))

             (u/declare ~fullname ~predicate-sym ~builtin-map-constructor-sym)

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
             (u/defr ~record-sym ~req-fields-names ~@body)

             (u/dof ~map-constructor-sym ~(map-constructor-form parsed #_#_builtin-map-constructor-sym fields))

             ;; constructors
             ~(if positional
                `(u/dof ~fullname ~(positional-constructor-form parsed #_#_builtin-positional-constructor-sym fields))
                `(u/dof ~fullname ~(unpositional-constructor-form parsed
                                                                  #_(if *cljs?* map-constructor-sym (u/dotsym->qualified-sym map-constructor-sym)))))

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
         (speculoos.utils/declare t4.a t4.a? )
         (do t4/a)
         (deft t4 {a {b [c :- number?]}})
         (macroexpand '(deft t4 {a {b number?}}))

         (deft t4 {a {b number?}})

         (t4 :a {:b "aze"})

         #_(do
           (speculoos.utils/declare t4 t4? map->R_t4)
           (clojure.spec.alpha/def :speculoos.types/t4 clojure.core/any?)
           (do
             (speculoos.utils/declare t4.a t4.a? map->R_t4_a)
             (clojure.spec.alpha/def :speculoos.types.t4/a clojure.core/any?)
             (speculoos.utils/with-dotsyms
               (clojure.spec.alpha/def :speculoos.types.t4.a/b number?)
               (macroexpand '(speculoos.utils/dof
                  t4.a.b?
                  (clojure.core/fn
                    [x__4019__auto__]
                    (clojure.core/when (clojure.spec.alpha/valid? number? x__4019__auto__) x__4019__auto__))))
               (speculoos.utils/dof
                 t4.a.b
                 (clojure.core/fn
                   [x__4013__auto__]
                   (clojure.core/let
                     [conformed__4014__auto__ (clojure.spec.alpha/conform number? x__4013__auto__)]
                     (clojure.core/when-not (clojure.spec.alpha/invalid? conformed__4014__auto__) conformed__4014__auto__)))))
             (clojure.spec.alpha/def
               :speculoos.types.t4/a
               (clojure.core/->
                 (speculoos.specs/spec->SpecImpl (clojure.spec.alpha/keys :req-un [:speculoos.types.t4.a/b] :opt-un []))
                 (clojure.core/update
                   :gen
                   (fn*
                     [p1__14814__14828__auto__]
                     (clojure.core/fn
                       [& xs__14829__auto__]
                       (clojure.test.check.generators/fmap map->R_t4_a (clojure.core/apply p1__14814__14828__auto__ xs__14829__auto__)))))
                 (clojure.core/update
                   :conform
                   (fn*
                     [p1__14815__14830__auto__]
                     (clojure.core/fn
                       [s__14831__auto__ x__14832__auto__]
                       (clojure.core/let
                         [ret__14833__auto__ (p1__14815__14830__auto__ s__14831__auto__ x__14832__auto__)]
                         (if (clojure.spec.alpha/invalid? ret__14833__auto__) ret__14833__auto__ (map->R_t4_a ret__14833__auto__))))))))
             (speculoos.utils/defr R_t4_a [b])
             (speculoos.utils/dof
               t4.a.from-map
               (clojure.core/fn
                 [{:as G__15026, :keys [b]}]
                 (clojure.core/let
                   [b
                    (clojure.core/let
                      [x__4017__auto__ b conformed__4018__auto__ (clojure.spec.alpha/conform :speculoos.types.t4.a/b x__4017__auto__)]
                      (clojure.core/if-not
                        (clojure.spec.alpha/invalid? conformed__4018__auto__)
                        conformed__4018__auto__
                        (throw (new Exception (str (pr-str b) " cannot be conformed to " :speculoos.types.t4.a/b)))))]
                   (map->R_t4_a (clojure.core/merge G__15026 {:b b} (speculoos.utils/rem-nil-vals {}))))))
             (speculoos.utils/dof
               t4.a
               (clojure.core/fn [& xs__14810__auto__] (t4.a/from-map (clojure.core/apply clojure.core/hash-map xs__14810__auto__))))
             (speculoos.utils/dof t4.a? (clojure.core/fn [x__14832__auto__] (R_t4_a? x__14832__auto__)))
             (clojure.core/defmethod
               clojure.pprint/simple-dispatch
               R_t4_a
               [x__14832__auto__]
               (clojure.pprint/simple-dispatch
                 (clojure.core/cons (quote a) (clojure.core/map (clojure.core/partial clojure.core/get x__14832__auto__) [:b]))))
             (clojure.core/defmethod
               clojure.core/print-method
               R_t4_a
               [x__14826__auto__ w__14827__auto__]
               (clojure.core/print-method
                 (clojure.core/cons (quote t4.a) (clojure.core/mapcat clojure.core/identity x__14826__auto__))
                 w__14827__auto__)))
           (clojure.spec.alpha/def
             :speculoos.types/t4
             (clojure.core/->
               (speculoos.specs/spec->SpecImpl (clojure.spec.alpha/keys :req-un [:speculoos.types.t4/a] :opt-un []))
               (clojure.core/update
                 :gen
                 (fn*
                   [p1__14814__14828__auto__]
                   (clojure.core/fn
                     [& xs__14829__auto__]
                     (clojure.test.check.generators/fmap map->R_t4 (clojure.core/apply p1__14814__14828__auto__ xs__14829__auto__)))))
               (clojure.core/update
                 :conform
                 (fn*
                   [p1__14815__14830__auto__]
                   (clojure.core/fn
                     [s__14831__auto__ x__14832__auto__]
                     (clojure.core/let
                       [ret__14833__auto__ (p1__14815__14830__auto__ s__14831__auto__ x__14832__auto__)]
                       (if (clojure.spec.alpha/invalid? ret__14833__auto__) ret__14833__auto__ (map->R_t4 ret__14833__auto__))))))))
           (speculoos.utils/defr R_t4 [a])
           (speculoos.utils/dof
             t4.from-map
             (clojure.core/fn
               [{:as G__15027, :keys [a]}]
               (clojure.core/let
                 [a
                  (clojure.core/let
                    [x__4017__auto__ a conformed__4018__auto__ (clojure.spec.alpha/conform :speculoos.types.t4/a x__4017__auto__)]
                    (clojure.core/if-not
                      (clojure.spec.alpha/invalid? conformed__4018__auto__)
                      conformed__4018__auto__
                      (throw (new Exception (str (pr-str a) " cannot be conformed to " :speculoos.types.t4/a)))))]
                 (map->R_t4 (clojure.core/merge G__15027 {:a a} (speculoos.utils/rem-nil-vals {}))))))
           (speculoos.utils/dof
             t4
             (clojure.core/fn [& xs__14810__auto__] (t4/from-map (clojure.core/apply clojure.core/hash-map xs__14810__auto__))))
           (speculoos.utils/dof t4? (clojure.core/fn [x__14832__auto__] (R_t4? x__14832__auto__)))
           (clojure.core/defmethod
             clojure.pprint/simple-dispatch
             R_t4
             [x__14832__auto__]
             (clojure.pprint/simple-dispatch
               (clojure.core/cons (quote t4) (clojure.core/map (clojure.core/partial clojure.core/get x__14832__auto__) [:a]))))
           (clojure.core/defmethod
             clojure.core/print-method
             R_t4
             [x__14826__auto__ w__14827__auto__]
             (clojure.core/print-method
               (clojure.core/cons (quote t4) (clojure.core/mapcat clojure.core/identity x__14826__auto__))
               w__14827__auto__)))



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
         (ns iop.iop.iop
           (:refer-clojure :exclude [val]))

         (def a 1)
         (def val (fn [x] x))
         (ns-publics (the-ns 'iop.iop))
         (do a)

         (ns a.b.c
           (:require [iop.iop :refer :all]
                     [iop.iop.iop :refer :all]))

         (do val )

         (ns bop.bop)

         ())
