(ns aoc-solutions.core
     (:gen-class))

(defn line-as-vec
  [line]
  (let [l (clojure.string/split line #" ")]
    (vector (get l 1) (get l 7))))

(defn to-reduce-parent
  [m p]
  (assoc m (get p 1) (conj (get m (get p 1)) (get p 0))))

(defn to-reduce-children
  [m p]
  (assoc m (get p 0) (conj (get m (get p 0)) (get p 1))))

(defn update-dependencies
  [dependencies dependency]
  (reduce #(assoc %1 (get %2 0) (remove (partial = dependency) (get %2 1))) {} dependencies))

(defn update-dependencies-multiple
  [dependencies-map dependencies]
  (if (empty? dependencies)
    dependencies-map
    (update-dependencies-multiple (update-dependencies dependencies-map (first dependencies)) (rest dependencies))))

(defn find-ready
  [m]
  (map #(get %1 0) (filter #(empty? (get %1 1)) m)))

(defn get-capital-letters []
  (map (comp str char) (range 65 91)))

(defn get-first-ones  [m]
  (filter #(= nil (get m %1)) (distinct (reduce concat (vals children)))))

(defn remove-ready
  [m]
  (reduce #(assoc %1 (get %2 0) (get %2 1)) {} (filter #(not (= '() (get %1 1))) m)))

(defn build-string
  [m ready]
  (if (and (empty? m) (empty? ready))
    ""
    (let [sorted-ready (sort ready)
          first-ready (first sorted-ready)
          updated-m (update-dependencies m first-ready)
          updated-ready (filter #(not (= nil %1)) (concat (rest sorted-ready) (find-ready updated-m)))
          removed-ready (remove-ready updated-m)]
      (str first-ready (build-string removed-ready updated-ready)))))

(defn solution
  [input]
  (let [to-parent-map (reduce to-reduce-parent {} input)
        ready (get-first-ones to-parent-map)]
    (build-string to-parent-map ready)))

(defn count-seconds
  ([letter] (count-seconds letter (get-capital-letters) 61))
  ([letter all-letters seconds]
   (if (= letter (first all-letters))
     seconds
     (count-seconds letter (rest all-letters) (inc seconds)))))

(defn add-to-threads [threads new-letters]
  (if (empty? new-letters)
    threads
    (add-to-threads (into threads {(first new-letters) (count-seconds (first new-letters))}) (rest new-letters))))

(defn remove-first-n [list n]
  (if (= 0 n)
    list
    (remove-first-n (rest list) (dec n))))

(defn get-finished [m]
  (filter #(not (= nil %)) (map #(if (= 0 (get % 1)) (get % 0)) m)))

(defn- process-current [m]
  (into {} (for [[k v] m] [k (dec v)])))

(defn free-workers [m]
  "removes threads whose work has been finished"
  (select-keys m (for [[k v] m :when (not (= v 0))] k)))

(defn second-solution
  "the first second is spent setting things up. That is why the result is 1 > correct"
  [seconds ready current children]
  (let [processed (process-current current)
        finished (get-finished processed)
        busy-threads (free-workers processed)
        free-threads-count (- 5 (count busy-threads))

        updated-children (update-dependencies-multiple children finished)
        temp-ready (sort (filter #(not (= nil %1)) (concat ready (find-ready updated-children))))
        new-ones (take free-threads-count temp-ready)
        updated-ready (remove-first-n temp-ready free-threads-count)
        updated-current (add-to-threads busy-threads new-ones)]
    (if (and (empty? updated-ready) (empty? updated-current))
      seconds
      (second-solution (inc seconds) updated-ready updated-current (remove-ready updated-children)))))


(defn -main [& args]
  (let [input (map line-as-vec (file-as-seq "resources/seventh-problem.txt"))
        children (reduce to-reduce-parent {} input)
        ready (get-first-ones children)]
    (second-solution 0 ready {} children)))
