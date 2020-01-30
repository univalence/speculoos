(ns speculoos.specs-t
  (:require [clojure.test :refer [deftest]]
            [clojure.spec.alpha :as s]
            [speculoos.utils :refer [is isnt]]
            [speculoos.specs :refer [spec->SpecImpl spec cpred one-of]]
            [clojure.spec.gen.alpha :as gen]))

(deftest one
  (is (list :explain :conform :unform :gen :with-gen :describe)
      (keys (spec->SpecImpl (s/spec integer?)))
      (keys (spec integer?))))

(s/def ::int-or-str (one-of integer? string?))

(deftest one-of-test
  (is 1 (s/conform ::int-or-str 1))
  (is "23" (s/conform ::int-or-str "23"))
  (is (s/conform ::int-or-str :iop) ::s/invalid)
  (every? #(or (string? %) (integer? %))
          (gen/sample (s/gen ::int-or-str) 10000)))

(s/def ::int!
  ;; cpred is like s/conformer but takes a function that returns either nil or a truthy value
  (cpred (fn [x] (when (number? x) (int x)))))

(deftest cpred-test
  (is 1
      (s/conform ::int! 1.2)
      (s/conform ::int! 1.9)
      (s/conform ::int! 1))
  (is (s/conform ::int! "12")
      ::s/invalid))




