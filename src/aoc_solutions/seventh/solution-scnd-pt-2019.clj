(ns aoc-solutions.core
  (:require [clojure.math.combinatorics :as combo])
  (:require [clojure.core.async
             :as a
             :refer [>! <! >!! <!! go chan buffer close! thread
                     alts! alts!! timeout]])
  (:gen-class))

(defn exec-phase-setting [perm program]
  (reduce #(amplifier program %1 %2) 0 perm))

(defn solution []
  (println "solution")
  (let [all-phase-settings (combo/permutations (range 5))
        phases-executed (map #(exec-phase-setting % input) all-phase-settings)]
    (reduce max phases-executed)))

(def ^:dynamic *channels* nil)

(defn third-op-chan [program counter output current-base operand]
  (let [input-chan (*channels* (last (drop-last program)))
       dinput (<!! input-chan)
        ]
       {:program (assoc program operand dinput)
        :output output
        :current-base current-base
        :counter (+ 2 counter)}))

(defn fourth-op-chan [program counter output current-base operand]
  (let [out-chan (*channels* (last program))]
    (>!! out-chan operand)
    {:program program ;(conj program operand)
     :output (conj output operand)
     :current-base current-base
     :counter (+ 2 counter)}))

(def third-op third-op-chan)
(def fourth-op fourth-op-chan)

(defn scnd-pt-exec-phase-setting [phases program]
  (binding [*channels* [(chan 2) (chan 2) (chan 2) (chan 2) (chan 2)]]
    (doseq [i (range 5)] (>!! (*channels* i) (phases i)))
    (>!! (*channels* 0) 0)
    (let [first (go (execute input 0 1 0 0));(execute inchan outchant counter)
          second (go (execute input 1 2 0 0))
          third (go (execute input 2 3 0 0))
          fourth (go (execute input 3 4 0 0))
          fifth (go (execute input 4 0 0 0))]
      (<!! fifth))
    ))



(defn scnd-pt []
  (let [phases (combo/permutations (range 5 10))]
    (reduce max (map #(scnd-pt-exec-phase-setting % input) phases))))

(def input [3 8 1001 8 10 8 105 1 0 0 21 34 59 76 101 114 195 276 357 438 99999 3 9 1001 9 4 9 1002 9 4 9 4 9 99 3 9 102 4 9 9 101 2 9 9 102 4 9 9 1001 9 3 9 102 2 9 9 4 9 99 3 9 101 4 9 9 102 5 9 9 101 5 9 9 4 9 99 3 9 102 2 9 9 1001 9 4 9 102 4 9 9 1001 9 4 9 1002 9 3 9 4 9 99 3 9 101 2 9 9 1002 9 3 9 4 9 99 3 9 101 2 9 9 4 9 3 9 1002 9 2 9 4 9 3 9 1002 9 2 9 4 9 3 9 1001 9 1 9 4 9 3 9 102 2 9 9 4 9 3 9 101 1 9 9 4 9 3 9 1001 9 1 9 4 9 3 9 1002 9 2 9 4 9 3 9 1001 9 2 9 4 9 3 9 102 2 9 9 4 9 99 3 9 101 2 9 9 4 9 3 9 1002 9 2 9 4 9 3 9 1001 9 1 9 4 9 3 9 101 2 9 9 4 9 3 9 1002 9 2 9 4 9 3 9 102 2 9 9 4 9 3 9 101 1 9 9 4 9 3 9 1001 9 2 9 4 9 3 9 101 2 9 9 4 9 3 9 1001 9 1 9 4 9 99 3 9 102 2 9 9 4 9 3 9 1001 9 1 9 4 9 3 9 1001 9 1 9 4 9 3 9 102 2 9 9 4 9 3 9 1001 9 2 9 4 9 3 9 1002 9 2 9 4 9 3 9 102 2 9 9 4 9 3 9 102 2 9 9 4 9 3 9 101 1 9 9 4 9 3 9 101 2 9 9 4 9 99 3 9 1002 9 2 9 4 9 3 9 102 2 9 9 4 9 3 9 102 2 9 9 4 9 3 9 102 2 9 9 4 9 3 9 1002 9 2 9 4 9 3 9 1002 9 2 9 4 9 3 9 101 2 9 9 4 9 3 9 102 2 9 9 4 9 3 9 101 2 9 9 4 9 3 9 101 2 9 9 4 9 99 3 9 1002 9 2 9 4 9 3 9 1001 9 1 9 4 9 3 9 101 2 9 9 4 9 3 9 101 2 9 9 4 9 3 9 101 2 9 9 4 9 3 9 101 1 9 9 4 9 3 9 1002 9 2 9 4 9 3 9 1002 9 2 9 4 9 3 9 1001 9 1 9 4 9 3 9 101 2 9 9 4 9 99])
