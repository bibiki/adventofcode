(ns aoc-solutions.fifteenth.solution-permutations
  (:require [clojure.math.combinatorics :as combo]
            [aoc-solutions.fifth.day-five-2019 :as fifth]
            [clojure.string :as s]))

(def paths (atom (combo/selections [1 2 3 4] 40)))
(def current-path (atom '()))
(def direction-count (atom 0))
(def visited-path (atom '()))

(defn input-function [program counter output current-base operand]
  (let [next-dir (atom (first (drop @direction-count @current-path)))
        o (last output)]
    (swap! direction-count inc)
    (swap! visited-path concat (list @next-dir))
    (print o "o de")
    (case o
      nil (print "droid started")
      0 (do (reset! next-dir nil))
      1 (do (print ""))
      2 (do (println "found system")
            (reset! next-dir nil)))
;    (println operand @next-dir @steps @direction-count)
    {:program (assoc program operand @next-dir)
     :counter (+ 2 counter)
     :output output
     :current-base current-base}))

(defn get-op-function [o]
  ([fifth/first-op fifth/second-op
    input-function fifth/fourth-op
    fifth/jump-if-true fifth/jump-if-false
    fifth/less-than fifth/equalss
    fifth/relative-base] (dec o)))

(defn get-intcode-program [filename]
  (vec (map read-string (s/split (slurp filename) #","))))

(defn- start-the-same? [start path]
  (let [p (take (count start) path)]
    (= p start)))

(defn shortestpath [one other]
  (if (= 0 one)
    other
    (min one other)))

(defn start-the-same? [one other]
  (let [c (count one)
        o (take c other)]
    (= one o)))

(defn solution [intcode getopfunction shortest r]
  (print r "-")
  (if (nil? @current-path)
    shortest
    (let [pathlength (count (fifth/execute intcode 0 [] 0 get-op-function 1))
          updated-paths (drop-while #(start-the-same? (drop-last @visited-path) %) @paths)
          next-path (first updated-paths)
          updated-shortest (shortestpath shortest pathlength)]
      (reset! current-path next-path)
      (reset! paths updated-paths)
      (reset! visited-path '())
      (recur intcode get-op-function shortest (inc r)))))

(defn run-solution []
  (let [intcode (vec (map read-string (s/split (slurp "resources/day-fifteen.txt") #",")))]
    (reset! current-path (first @paths))
    (reset! paths (drop 1 @paths))
    (solution intcode get-op-function 0 1)))
