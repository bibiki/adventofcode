(ns aoc-solutions.core
  (:require [clojure.math.combinatorics :as combo]))

;I need to implement my input function to provide droid movement instructions
;the program should stop when droid returns in base
;the droid should not move forward if it gets it in a position already visited
;the droid should move back to position it came from if it has explored all other three directions
;for droid to know where to move back to, I need to keep track of from-to pairs of positions
;when droid reports a 2, I need to record how many steps have been taken, to find the shortest path from base to where the system I am searching is at. If earlier number of steps is more than current number of steps, replace the path and continue just as if 1 was reported
;to count number of steps, add one every time a step is taken forward, and subtract one every time a step back is taken.

;but how do I most efficiently design a logic to give instructions for how to move?
(def to-from-hist (atom {[0 0] nil}))
(def pos-dirs-hist (atom {[0 0] '(1 2 3 4)}))
(def steps (atom 0))
(def cpos (atom [0 0]))
(def cdir (atom 1))

(defn- calc-back [from to]
  (let [dir (vec (map #(- %1 %2) to from))]
    (case dir
      [-1 0] 4
      [1 0] 3
      [0 -1] 1
      [0 1] 2)))

(defn- go-back [cpos hist]
  "this should only be called from get-next-dir, never from higher up hierarchy"
  (let [from (get hist cpos)]
    (calc-back from cpos)))

(defn next-pos [cpos dir]
  (let [x (first cpos)
        y (last cpos)]
    (case dir
      4 (vector (inc x) y)
      3 (vector (dec x) y)
      2 (vector x (dec y))
      1 (vector x (inc y)))))

(defn get-next-dir [cpos hist dirs]
;  (println "getting next dir")
  (let [unexplored (filter #(not= nil (get hist (next-pos cpos %))) dirs)]
    (if (empty? unexplored)
      (go-back cpos hist)
      (first unexplored))))

(defn init-new-pos-in-hist []
  (let [nextpos (next-pos @cpos @cdir)
        back (calc-back @cpos nextpos)
        filtered (filter #(= nil (get @pos-dirs-hist (next-pos @cpos %))) '(4 3 2 1))
        dirs (filter #(not= % back) filtered)
        updated-dirs (concat dirs (list back))
        hist (assoc @pos-dirs-hist nextpos updated-dirs)]
    (if (nil? (get @pos-dirs-hist nextpos))
      (reset! pos-dirs-hist hist))))


(defn check-dir-explored [cpos cdir pos-dirs-hist]
  (let [dirs (get pos-dirs-hist cpos)
        new-dirs (filter #(not= cdir) (dirs))]
    (assoc pos-dirs-hist cpos new-dirs)))

(defn mark-as-visited []
  (let [cp @cpos
        dir @cdir
        nextp (next-pos cp dir)
        update-hist (assoc @to-from-hist nextp cp)]
    (reset! to-from-hist update-hist)))

(defn try-next-direction []
  (let [dirs (get @pos-dirs-hist @cpos)
        dir (first dirs)
        updated-dirs (drop 1 dirs)
        updated-hist (assoc @pos-dirs-hist @cpos updated-dirs)]
    (if (= 1 (count dirs))
      (reset! pos-dirs-hist (assoc @pos-dirs-hist @cpos dirs))
      (reset! pos-dirs-hist updated-hist))
    (reset! cdir dir)))

(defn add-step []
  (swap! steps inc))

(defn should-go-back? []
  (let [available-dirs (get @pos-dirs-hist @cpos)]
    (< (count available-dirs) 2)))

(defn move-step-back []
  (swap! steps dec)
  (swap! steps dec))

(defn move-to-next-pos []
  (let [next-pos (next-pos @cpos @cdir)]
    (reset! cpos next-pos)))

(defn m-input [program counter output base operand]
;  (println output "aint no sunshine" @pos-dirs-hist)
  (let [o (last output)]
;    (println "only darkness" o @cpos @cdir)
       (case o
         nil (println "started droid")
         0 (do
;             (println "we had a wall" (get @pos-dirs-hist @cpos) @cpos)
             (mark-as-visited)
             (try-next-direction))
         1 (do
 ;            (println "we had a 1" @cpos @cdir)
             (mark-as-visited)
             (init-new-pos-in-hist);fixed
             (move-to-next-pos)
             (add-step)
             (if (should-go-back?) (do (println "going back") (move-step-back)))
             (try-next-direction))
         2 (do
             (add-step)
             (move-to-next-pos)
             (println "found system " @cpos @steps)
             (try-next-direction)))
       {:program (assoc program operand @cdir)
        :ouput output
        :current-base base
        :counter (+ 2 counter)}))

(def third-op m-input)

(def input (vec (map read-string (clojure.string/split (slurp "resources/day-fifteen.txt") #","))))

(defn m [] (execute input 0 [] 0))
