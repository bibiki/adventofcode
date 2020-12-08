(ns aoc-solutions.core
  (:require [clojure.string :as s]))


(defn get-list [filename]
  (map #(s/split % #" => ") (s/split (slurp filename) #"\n")))

(defn find-combo [combo list]
  (first (filter #(= combo (last (s/split (last %) #" "))) list)))

(defn dumb [q c]
  (let [c-split (s/split c #" ")
        c-a (read-string (first c-split))
        c-name (last c-split)]
    (str (* q c-a) " " c-name)))

(defn is-derivative [derivative base]
  (reduce #(or %1 %2) false (map #(.contains derivative %) base)))

(defn distance [combo list current]
; (println combo "de")
  (if (= "ORE" (first combo)) 100
      (if (= ["FUEL"] combo)
        current
        (let [derivatives (filter #(is-derivative (first %) combo) list)]
                                        ;      (println "derivatives" derivatives)
          (recur (distinct (map #(last (s/split (last %) #" ")) derivatives)) list (inc current))))))

(defn- sum [group]
  (let [to-sum (last group)
        to-num (map #(read-string (first (s/split % #" "))) to-sum)]
    (reduce + to-num)))

(defn sum-similar [combos]
  (let [grouped (group-by #(last (s/split % #" ")) combos)
        summer (map #(vector (first %) (sum %)) (seq grouped))
        rez (map #(str (last %) " " (first %)) summer)]
    rez))

(defn map-to-ingredients [combo list]
; (println combo "crude")
  (if (= "ORE" (last (s/split combo #" ")))
    [combo]
    (let [combo-split (s/split combo #" ")
          combo-amount (read-string (first combo-split))
          combo-name (last combo-split)
          formula (find-combo combo-name list)
          ingridients (s/split (first formula) #", ")
          combo-in-question (last formula)
          combo-in-question-amount (read-string (first (s/split combo-in-question #" ")))
          quota (Math/ceil (/ combo-amount combo-in-question-amount))]
      (map #(dumb quota %) ingridients))))

(defn sorter [one other list]
  (let [done (distance [(last (s/split one #" "))] list 0)
        dother (distance [(last (s/split other #" "))] list 0)]
    (< done dother)))

(defn reducer [accumulator l ll]
  (if (nil? l)
    accumulator
    (sum-similar (concat accumulator (map-to-ingredients l ll)))))

(defn calc-ore [combos list]
  (if (= 0 (count (filter #(not (.contains % "ORE")) combos)))
    combos
    (let [sorted (sort #(sorter %1 %2 list) combos)
          f (first sorted)
          f-mapped (map-to-ingredients f list)
 ;         ddd (println f-mapped f)
          without-first (drop 1 sorted)
          add-f-mapped (concat without-first f-mapped)
          summed (sum-similar add-f-mapped)]
      (recur summed list))))

(defn- calc-ore-int [combos list]
  (read-string (first (s/split (first (calc-ore combos list)) #" "))))

(defn seek [resources range-left range-right list]
  (if (= 1 (- range-right range-left))
    range-right
    (let [mid (quot (+ range-left range-right) 2)
          calc (calc-ore-int [(str mid " FUEL")] list)]
      (if (> calc resources)
        (recur resources range-left mid list)
        (recur resources mid range-right list)))))

(defn calc
  ([schemes]
   (let [fuel-eq (filter #(= "1 FUEL" (last %)) schemes)]
     (calc fuel-eq schemes)))
  ([fuel schemes]))
