(ns speculoos.all
  (:gen-class)
  (:require [clojure.test :refer :all]
            [speculoos.flow-t]
            [speculoos.specs-t]
            [speculoos.types-t]
            [speculoos.patterns-t]
            [speculoos.specs-t]
            [speculoos.utils-t]))

(defn -main []
  (run-tests 'speculoos.flow-t
             'speculoos.specs-t
             'speculoos.types-t
             'speculoos.patterns-t
             'speculoos.specs-t
             'speculoos.utils-t))
