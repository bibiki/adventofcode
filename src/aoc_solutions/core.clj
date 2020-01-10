(ns aoc-solutions.core
  (:gen-class))
;; how to make an indefinite seq out of a file's lines?
;; when EOF is reached, seq should go back to first line in file

;; how to use desctructuring?

;; how to break a string into characters

;; how to break a new-line delimited list of strings - when slurping a file

;; how to pass the filename to slurp? the root of the project is the directory that contains src and resources directoris

;; variable-arity methods - how to define them

;; how to check if a seqable contains a given element

;; how to separate solutions to aoc puzzles a file per problem?

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(defn first-solution
  []
  (reduce + (map #(Integer. %) (clojure.string/split-lines (slurp "src/aoc_solutions/first_problem/my-input.txt")))))

(defn- find-first-repeated-sum
  [nums c-nums current-sum prev-sums]
  (if (empty? nums)
    (find-first-repeated-sum c-nums c-nums current-sum prev-sums)
    (if (some #{(+ (first nums) current-sum)} prev-sums)
      (+ (first nums) current-sum)
      (recur
       (rest nums)
       c-nums
       (+ (first nums) current-sum)
       (cons (+ (first nums) current-sum) prev-sums)))))

(defn first-solution-part-b
  []
  (find-first-repeated-sum
   (map #(Integer. %) (clojure.string/split-lines (slurp "src/aoc_solutions/first_problem/my-input.txt")))
   (map #(Integer. %) (clojure.string/split-lines (slurp "src/aoc_solutions/first_problem/my-input.txt")))
   0
   '(0)))

(defn read-lines
  [filename]
  (clojure.string/split-lines (slurp filename)))

(defn exactly-twice?
  ([how-many word] (exactly-twice? how-many (map identity word) word))
  ([how-many characters word]
   (if (not (empty?  characters))
     (if (= how-many (reduce + (map #(if (= % (first characters)) 1 0) word)))
       word
       (exactly-twice? how-many (rest characters) word))
     false)))

(defn second-solution
  [filename]
  (let [lines (read-lines filename) 
        twos (filter #(exactly-twice? 2 %) lines)
        threes (filter #(exactly-twice? 3 %) lines)]
    (* (count twos) (count threes))))

(defn word-distance
  [w1 w2 distance]
  (if (or (empty? w1) (empty? w2))
    distance
    (let [[w1f & w1r] w1 [w2f & w2r] w2]
      (if (= w1f w2f)
        (recur w1r w2r distance)
        (recur w1r w2r (inc distance))))))

(defn has-word-distance?
  [how-many word words]
  (if (empty? words)
    false
    (if (= how-many (word-distance word (first words) 0))
      word
      (has-word-distance? how-many word (rest words)))))

(defn second-solution-b
  [filename]
  (let [lines (read-lines filename)]
    (filter #(has-word-distance? 1 % lines) lines)))

(defn init-vector
  ([length value default offset end]
   (init-vector length value default offset end 0))
  ([length value default offset end current]
   (if (= 0 length) []
       (let [this-value (if (and (>= current offset) (< current (+ offset end))) value default)]
         (cons this-value (init-vector (- length 1) value default offset end (+ current 1))))))
  ([length value]
   (if (= 0 length) []
       (cons value (init-vector (- length 1) value)))))

(defn init-matrix
  ([rows cols value]
   (init-vector rows (init-vector cols value)))
  ([rows cols value default roffset coffset rend cend]
   (let [z-vector (init-vector cols default)
         zero-one-vector (init-vector cols value default coffset cend)]
     (init-vector rows zero-one-vector z-vector roffset rend))))

(defn add-matrices
  ([m1 m2]
   (map #(map + %1 %2) m1 m2))
  ([m] (add-matrices m (init-matrix (count m) (count (first m)) 0))))

(defn- zeros-and-ones
  "Reads in a row of numbers and transforms its elements to 0 if they ae greater than 1, and to otherwise"
  [row]
  (map #(if (< 1 %) 1 0) row))

(defn count-contended-inches
  [m]
  (reduce + 
          (map #(reduce + %) (map zeros-and-ones m))))

;; (zipmap '(:one :two :rowoff :rowend :ignore :coloff :colend) (clojure.string/split "#1 @ 335,861: 14x10" #" |\,|x|\:"))
(defn to-matrix
  [{:keys [rowoff coloff rowlen collen]}]
  (init-matrix 1000 1000 1 0 (Integer. rowoff) (Integer.  coloff) (Integer. rowlen) (Integer. collen)))

(defn third-solution
  [filename]
  (let [lines (read-lines filename)
        matrices (map to-matrix lines)]
    (count-contended-inches (reduce add-matrices matrices))))

(defn line-to-map
  [line]
  (zipmap '(:rowoff :coloff :rowlen :collen) (map #(Integer. %) (filter #(not (empty? %)) (next (next (clojure.string/split line #" |\,|x|\:")))))))

(defn add-as-matrix
  ([line] (add-matrices (to-matrix line)))
  ([matrix line] (add-matrices matrix (to-matrix line))))


(defn solution-3
  [m [head & tail]]
  (if (= nil head)
    m
    (recur (add-matrices m (to-matrix (line-to-map head))) tail)))

(defn file-as-seq
  [filename]
  (line-seq (clojure.java.io/reader filename)))



