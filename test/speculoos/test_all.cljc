(ns speculoos.test-all
  #_(:gen-class)
  (:require
    #?(:clj  [clojure.test :refer [run-tests]]
       :cljs [cljs.test :refer-macros [run-tests]])
    [speculoos.flow-t]
    [speculoos.specs-t]
    [speculoos.types-t]
    [speculoos.patterns-t]
    [speculoos.specs-t]
    [speculoos.utils-t]))

(defn -main [& _]
  (run-tests 'speculoos.flow-t
             'speculoos.specs-t
             'speculoos.types-t
             'speculoos.patterns-t
             'speculoos.specs-t
             'speculoos.utils-t))
