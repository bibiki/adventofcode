(ns aoc-solutions.core
  (:gen-class)
  (:require [clojure.set :refer [intersection]]))

(defn gravity [p ps]
  (let [gravities (map #(compare % p) ps)]
    (reduce + gravities)))

(defn gravities [ps]
  (map #(gravity % ps) ps))

(defn apply-velocity [ps velocities]
  (map + ps velocities))

(defn add-history [p step history]
;  (println p step history)
  (assoc-in history p (conj (get-in history p #{}) step)))

(defn add-rows [r1 r2]
  (vec (map + r1 r2)))

(defn solve [ps vs phistory vhistory step]
;  (println ps)
;  (println "-------------------" (count vhistory) (count phistory))
  (let [p-steps (map #(get-in %2 %1 #{}) ps phistory)
        v-steps (map #(get-in %2 %1 #{}) vs vhistory)
        vsteps (reduce clojure.set/intersection v-steps)
        psteps (reduce clojure.set/intersection p-steps)]
    (if (= 2 (count (clojure.set/intersection vsteps psteps)))
      step
      (let [gravities (map gravities ps)
            velocities (map add-rows gravities vs)
            new-ps (map add-rows ps velocities)
            p-history (map #(add-history %1 step %2) ps phistory)
            v-history (map #(add-history %1 step %2) vs vhistory)
            next-step (inc step)]
        (recur new-ps velocities p-history v-history next-step)))))


(defn count-cycle [xs vs history count]
  (if (contains? history (concat xs vs))
    count
    (let [gs (gravities xs)
          n-vs (map + gs vs)
          n-xs (map + n-vs xs)]
      (recur n-xs n-vs (conj history (concat xs vs)) (inc count)))))

(defn tt [ps vs]
  (let [x-cycle (count-cycle (nth ps 0) (nth vs 0) #{} 0)
        y-cycle (count-cycle (nth ps 1) (nth vs 1) #{} 0)
        z-cycle (count-cycle (nth ps 2) (nth vs 2) #{} 0)]
    (list x-cycle y-cycle z-cycle)))

(def test-data '((-1 2 4 3) (0 -10 -8 5) (2 -7 8 -1)))

(def actual [[1 -14 -4 6] [-4 9 -6 -9] [3 -4 7 -11]])

(defn run []
  (tt actual '((0 0 0 0) (0 0 0 0) (0 0 0 0))))
