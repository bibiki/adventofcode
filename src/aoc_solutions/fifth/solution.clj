(ns aoc-solutions.core
  (:require [clojure.string :refer :all])
  (:gen-class))


(defn solution
  "this is a recursive solution. gives StackOverflowException for large input"
  ([line] (solution (subs line 0 1) (subs line 1)))
  ([f r]
   (if (empty? r)
     f
     (let [fr (subs r 0 1)]
       (if (or (ends-with? f fr) (not (ends-with? (lower-case f) (lower-case fr))))
         (solution (str f fr) (subs r 1))
         (solution (subs f 0 (- (count f) 1)) (subs r 1)))))))


(defn solution-l
  [line]
  (loop [f (subs line 0 1) r (subs line 1)]
      (if (empty? r)
     f
     (let [fr (subs r 0 1)]
       (if (or (ends-with? f fr) (not (ends-with? (lower-case f) (lower-case fr))))
         (recur (str f fr) (subs r 1))
         (recur (subs f 0 (- (count f) 1)) (subs r 1)))))))


(defn get-all-units-capitalized
  []
  (map (comp str char) (range 65 91)))

(defn replace-all-units
  [line units]
  (map #(replace (replace line %1 "") (lower-case %1) "") units))

(defn react-all-and-count
  [lines]
  (map (comp count solution-l) lines))

(defn second-part
  [line]
  (reduce min
          (react-all-and-count (replace-all-units line (get-all-units-capitalized)))))

;during today's solutions, I had to map over maps, compare somehow vectors, find max value ine a map which comparing vectors, replace substrings inside a string, find substring inside a string, check if a string contains a substring. Very interestingly, I implemented a function by overloading it four or five times (solution to problem 4), I had to use loops as recursion was giving me stack overflow exception
