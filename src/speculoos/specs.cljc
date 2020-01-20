(ns speculoos.specs
  (:refer-clojure :exclude [defrecord])
  (:require #?(:clj  [clojure.core :as c] :cljs [cljs.core :as c])
            #?(:clj  [clojure.spec.alpha :as s] :cljs [cljs.spec.alpha :as s])
            [spec-tools.data-spec :as ds]
            [clojure.test.check.generators :as tcg]
            [clojure.spec.gen.alpha :as gen]
            [speculoos.utils :as u]
            #?(:clj [speculoos.state :refer [*cljs?*]]))
  #?(:cljs (:require-macros [speculoos.specs :refer [defspec]])))

#?(:clj

   (do :spec-forms

       (defn spec-sym [x]
         (symbol
           (if *cljs?*
             "cljs.spec.alpha"
             "clojure.spec.alpha")
           (name x)))

       (defn conformer-form
         ([s]
          `(fn [x#]
             (let [conformed# (~(spec-sym "conform") ~s x#)]
               (when-not (~(spec-sym "invalid?") conformed#)
                 conformed#))))
         ([s x]
          `(let [x# ~x
                 conformed# (~(spec-sym "conform") ~s x#)]
             (when-not (~(spec-sym "invalid?") conformed#)
               conformed#))))

       (defn validator-form
         ([s]
          `(fn [x#]
             (when (~(spec-sym "valid?") ~s x#) x#)))
         ([s x]
          `(let [x# ~x]
             (when (~(spec-sym "valid?") ~s x#) x#)))))

   )

;; when creating a spec, or reifying the spec protocol
;; what we get are opaque objects, not allowing implementation sharing or composition
;; with the SpecImpl record you have something that behaves exactely like a Spec but expose its implementations

(c/defrecord SpecImpl
  [explain conform unform gen with-gen describe]

  #?@(:clj [s/Specize
            (specize* [s] s)
            (specize* [s _] s)])
  s/Spec
  (explain* [s path via in x] (explain s path via in x))
  (conform* [s x] (conform s x))
  (unform* [s x] (unform s x))
  (gen* [s overrides path rmap] (gen s overrides path rmap))
  (with-gen* [s g] (with-gen s g))
  (describe* [s] (describe s)))

(defn spec->SpecImpl
  "wrap any kind of specizable thing into a SpecImpl record"
  [s]
  (let [s (s/specize* s)]
    (SpecImpl.
      (fn [_ path via in x] (s/explain* s path via in x))
      (fn [_ x] (s/conform* s x))
      (fn [_ x] (s/unform* s x))
      (fn [_ overrides path rmap] (s/gen* s overrides path rmap))
      (fn [_ g] (s/with-gen* s g))
      (fn [_] (s/describe* s)))))

(def invalid?
  #{:clojure.spec.alpha/invalid
    :cljs.spec.alpha/invalid})

(def invalid
  #?(:clj  :clojure.spec.alpha/invalid
     :cljs :cljs.spec.alpha/invalid))

(def spec0
  (assoc
    (spec->SpecImpl
      (s/spec-impl
        'any
        identity
        (constantly (s/gen any?))
        true
        identity))
    :form 'any
    :explain
    (fn [s path via in x]
      (when (invalid? (s/conform s x))
        [{:path path :pred (:form s) :val x :via via :in in}]))
    :gen
    (fn [s & _] (u/error "no gen for: " s))))

(defn spec

  ([x]
   (cond
     (map? x) (merge spec0 x)
     (fn? x) (spec {:conform x})
     :else (u/error "conform-fn | spec-impl-map \n had: \n" x)))

  ([conform gen]
   (spec {:conform conform
          :gen (if (tcg/generator? gen)
                 (fn [& _] gen)
                 gen)}))

  ([conform gen form & xs]
   (apply assoc (spec conform gen)
          :form form
          xs)))

#?(:clj (defmacro defspec [n & xs]
          (if (:ns &env)
            `(cljs.spec.alpha/def ~(keyword (str *ns*) (name n)) (spec ~@xs))
            `(clojure.spec.alpha/def ~(keyword (str *ns*) (name n)) (spec ~@xs)))))

(defn wrap-gen&conform

  [spec f]

  (-> (spec->SpecImpl spec)

      (update :gen
              #(fn [& xs] (tcg/fmap f (apply % xs))))

      (update :conform
              #(fn [s x]
                 (let [ret (% s x)]
                   (if (invalid? ret)
                     ret (f ret)))))))

(comment
  (clojure.core/defrecord Foo [values])

  (let [s (wrap-gen&conform
            (ds/spec ::foo
                     {:values (s/map-of string? integer?)})
            map->Foo)]

    (gen/generate (s/gen s))))

(defn gen1 [spec]
  (gen/generate (s/gen spec)))

#_(cljs.pprint/pprint (s/registry))
