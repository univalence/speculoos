(ns speculoos.test-all
  #_(:gen-class)
  (:require [clojure.test #?(:clj :refer :cljs :refer-macros) [run-tests]]
            [speculoos.flow-t]
            [speculoos.specs-t]
            [speculoos.types-t]
            [speculoos.patterns-t]
            [speculoos.specs-t]
            [speculoos.utils-t]))

(println "yop")

(defn -main [& _]
  (run-tests 'speculoos.flow-t
             'speculoos.specs-t
             'speculoos.types-t
             'speculoos.patterns-t
             'speculoos.specs-t
             'speculoos.utils-t))
