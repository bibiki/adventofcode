(ns aoc-solutions.core
  (:require [clojure.math.combinatorics :as combo])
  (:require [clojure.core.async
             :as a
             :refer [>! <! >!! <!! go chan buffer close! thread
                     alts! alts!! timeout]])
  (:gen-class))

(defn amplifier [program i phase]
  (let [program-with-input (conj (conj program i) phase)
        counter 0]
    (execute program-with-input counter)))

(defn exec-phase-setting [perm program]
  (reduce #(amplifier program %1 %2) 0 perm))


(def input [3 8 1001 8 10 8 105 1 0 0 21 34 59 76 101 114 195 276 357 438 99999 3 9 1001 9 4 9 1002 9 4 9 4 9 99 3 9 102 4 9 9 101 2 9 9 102 4 9 9 1001 9 3 9 102 2 9 9 4 9 99 3 9 101 4 9 9 102 5 9 9 101 5 9 9 4 9 99 3 9 102 2 9 9 1001 9 4 9 102 4 9 9 1001 9 4 9 1002 9 3 9 4 9 99 3 9 101 2 9 9 1002 9 3 9 4 9 99 3 9 101 2 9 9 4 9 3 9 1002 9 2 9 4 9 3 9 1002 9 2 9 4 9 3 9 1001 9 1 9 4 9 3 9 102 2 9 9 4 9 3 9 101 1 9 9 4 9 3 9 1001 9 1 9 4 9 3 9 1002 9 2 9 4 9 3 9 1001 9 2 9 4 9 3 9 102 2 9 9 4 9 99 3 9 101 2 9 9 4 9 3 9 1002 9 2 9 4 9 3 9 1001 9 1 9 4 9 3 9 101 2 9 9 4 9 3 9 1002 9 2 9 4 9 3 9 102 2 9 9 4 9 3 9 101 1 9 9 4 9 3 9 1001 9 2 9 4 9 3 9 101 2 9 9 4 9 3 9 1001 9 1 9 4 9 99 3 9 102 2 9 9 4 9 3 9 1001 9 1 9 4 9 3 9 1001 9 1 9 4 9 3 9 102 2 9 9 4 9 3 9 1001 9 2 9 4 9 3 9 1002 9 2 9 4 9 3 9 102 2 9 9 4 9 3 9 102 2 9 9 4 9 3 9 101 1 9 9 4 9 3 9 101 2 9 9 4 9 99 3 9 1002 9 2 9 4 9 3 9 102 2 9 9 4 9 3 9 102 2 9 9 4 9 3 9 102 2 9 9 4 9 3 9 1002 9 2 9 4 9 3 9 1002 9 2 9 4 9 3 9 101 2 9 9 4 9 3 9 102 2 9 9 4 9 3 9 101 2 9 9 4 9 3 9 101 2 9 9 4 9 99 3 9 1002 9 2 9 4 9 3 9 1001 9 1 9 4 9 3 9 101 2 9 9 4 9 3 9 101 2 9 9 4 9 3 9 101 2 9 9 4 9 3 9 101 1 9 9 4 9 3 9 1002 9 2 9 4 9 3 9 1002 9 2 9 4 9 3 9 1001 9 1 9 4 9 3 9 101 2 9 9 4 9 99])