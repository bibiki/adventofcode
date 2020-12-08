(ns aoc-solutions.core)

;this solves the first part of the puzzle
;first it adds 0 to the end of input to set mode of execution
;it runs the program to get its output
;since the game draws tiles with three output instructons, I group them in lists of three
;I filter those lists of three to identify those that drew block tiles (= 2 (last %))

(defn solve-first-part []
  (let [input (vec (map read-string (clojure.string/split (slurp "day-thirteen.txt") #",")))
        add-mode-input (conj input 0)
        output (execute add-mode-input 0 [] 0)
        partitioned (partition 3 output)
        filter-in-blocks (filter #(= 2 (last %)) partitioned)]
    (count filter-in-blocks)))

;;set up for second part of this puzzle
(def coordinates (atom []))

(def ball (atom []))

(def paddle (atom []))

(def cntr (atom 0))

(defn read-input []
  (if (= (@ball 0) (@paddle 0))
    0
    (if (> (@ball 0) (@paddle 0))
      1
      -1)))

(defn input-op [program counter output current-base operand]
  {:program (assoc program operand (read-input))
   :output output
   :current-base current-base
   :counter (+ 2 counter)})

(defn output-op [program counter output current-base operand]
  (swap! cntr inc)
  (if (= 0 (rem @cntr 3))
    (let [coords @coordinates]
      (reset! coordinates [])
      (if (= 4 operand)
        (reset! ball coords)
        (if (= 3 operand)
          (reset! paddle coords))))
    (swap! coordinates conj operand))
  {:program program
   :output (conj output operand)
   :current-base current-base
   :counter (+ 2 counter)})

(def fourth-op output-op)
(def third-op input-op)

(defn second-part-solution []
  (let [input (vec (map read-string (clojure.string/split (slurp "day-thirteen.txt") #",")))
        add-mode-input (conj input 0)
        output (execute add-mode-input 0 [] 0)
        partitioned (partition 3 output)
        filter-in-blocks (filter #(= -1 (first %)) partitioned)]
;    (println partitioned)
    (last (last filter-in-blocks))))
