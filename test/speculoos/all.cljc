(ns speculoos.all
  (:require [clojure.test :refer :all]
            [speculoos.flow-t]
            [speculoos.specs-t]
            [speculoos.types-t]
            [speculoos.patterns-t]
            [speculoos.specs-t]
            [speculoos.utils-t]))

(defn test-all []
  (run-tests 'speculoos.flow-t
             'speculoos.specs-t
             'speculoos.types-t
             'speculoos.patterns-t
             'speculoos.specs-t
             'speculoos.utils-t))
