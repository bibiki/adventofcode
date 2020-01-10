(ns aoc-solutions.core
  (:gen-class))

(defn all-coords
  [length width]
  (map #(vector (quot %1 length) (rem %1 width)) (range (* length width))))

(defn manhattan-distance
  [v1 v2]
  (+ (Math/abs (- (first v1) (first v2))) (Math/abs (- (first (rest v1)) (first (rest v2))))))

(defn list-of-coordinates
  [filename]
  (let [coords (file-as-seq filename)]
    (map #(vec (map (fn [x] (Integer. (clojure.string/trim x))) (clojure.string/split %1 #","))) coords)))

(defn calc-md-from-to
  [point grid]
  (map #(manhattan-distance point %1) grid))

(defn calc-md-for-each
  [points grid]
  (map #(calc-md-from-to %1 grid) points))

(defn compare-two
  [one other]
  (map #(if (< %1 %2) 0 %1) one other))

(defn solution
  [t-calced]
  (map 
   #(reduce + %1) 
   (map
    flatten
    (reduce
     #(map vector %1 %2)
     (map
      #(if (= 1 (reduce + %1)) %1 '(0 0 0 0 0 0))
      (map
       (fn [x] (let [m (reduce min x)] (map #(if (= %1 m) 1 0) x)))
       (map flatten (reduce #(map vector %1 %2) t-calced))))))))

(defn min-two-points-val
  [x y]
  (let [f (get x 1)
           g (get y 1)]
       (if (<= f g)
         x
         y)))

(defn min-vect
  ([x] (min-vect x (first x)))
  ([x current]
   (if (= 1 (count x))
     (if (= nil current) (first x) (min-two-points-val (first x) current))
     (min-vect (rest x) (min-two-points-val (first x) current)))))

(defn min-as-vect
  [l]
  (let [m (min-vect (map-indexed vector l))
        how-many (count (filter #(= %1 (get m 1)) l))]
    (if (= how-many 1)
      m)))

(defn is-edge?
  [point grid-length]
  (or
   (= nil point)
   (< point grid-length)
   (>= point (* (- grid-length 1) grid-length))
   (= 0 (rem point grid-length))
   (= 0 (rem (+ point 1) grid-length))))

(defn -main
  [args]
  (def t-input (list-of-coordinates "resources/sixth-problem.txt"))
  (def t-grid (all-coords 360 360))
  (def t-calced (map #(calc-md-from-to %1 t-grid) t-input))
;  (println t-calced)
  ;rotates the  matrix that t-calced is
  ;effectively, t-calced is a list of grids where each grid
  ;marks distances of each point in it from a point given as input to task
  ;by rotating the matrix, we get a list of lists, where each list holds distances of a single point in a grid from each point given as input.
  ;by determening which index in these lists holds the smallest distance, we determine which input point's area includes the point that this list corresponds to
  (def x (map flatten (reduce #(map vector %1 %2) t-calced)))
  ;as explained above, the following min-as-vect finds the index in a list that holds the min of the list. if the list contains multiple copies of this min, nil is returned instead of the index
  (def xx (map min-as-vect x))
  ;xx will contain a list of vectors [a b] where a marks the index of the input point that includes in its' area the index of [a b] inside the containing list. b marks the distance between point identified by a and point identified by the index of [a b] inside the list
  (def f (map (fn [x] (if (= nil x) -1 (get x 0))) xx))
;  (def d (distinct (map #(get %1 1) (filter #(is-edge? (get %1 0) 360) (map-indexed vector f)))))
  (def d '(10 0 41 12 6 9 31 -1 34 45 27 20 19 49 33 43 1 18 47 48 5 26 39))
  (filter #(not (some (partial =) (get %1 0))) (frequencies (map (fn [x] (if (= nil x) -1 (get x 0))) xx))))
