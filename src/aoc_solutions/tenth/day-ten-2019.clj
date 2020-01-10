(ns aoc-solutions.core
  (:gen-class))

(defn grid [rows cols]
  ;(for [x (range 0 rows) y (range 0 cols)] [x y])
  (map #(vector (rem % rows) (quot % cols)) (range (* rows cols))))

(defn read-map [filename]
  (reduce into [] (map #(clojure.string/split % #"") (clojure.string/split (slurp filename) #"\n"))))

(defn get-asteroid-coordinates [grid asteroids]
  "39 is the width and height of grid, 1522 = 1 + 39 * 39. useful to generate a range that equal to number of points on grid"
  (let [grid-area (count grid)
        dimension (int (Math/sqrt grid-area))]
    (map #(vector (rem % dimension) (quot % dimension)) (filter #(= "#" (get asteroids %)) (range 0 (inc grid-area))))))

(defn find-all-slopes [p grid]
  "finds all possible slopes from point p to any point on grid except for those on the same y coordinate as that will be dividing by zero"
  (let [grid-without-p-coords (filter #(not (= (get p 0) (get % 0))) grid)]
    (distinct (map #(/ (- (get p 1) (get % 1)) (- (get p 0) (get % 0))) grid-without-p-coords))))

(defn find-c [p slope]
  (- (get p 1) (* slope (get p 0))))

(defn can-be-seen? [slope c p]
  (= (get p 1) (+ (* slope (get p 0)) c)))

(defn has-one-point? [slope c points]
  (if (empty? points)
    false
    (if (can-be-seen? slope c (first points))
      true
      (has-one-point? slope c (rest points)))))

(defn can-see-asteroid-up? [p asteroids]
  (not (empty? (filter #(and (< (get p 1) (get % 1)) (= (get p 0) (get % 0))) asteroids))))

(defn can-see-asteroid-down? [p asteroids]
  (not (empty? (filter #(and (> (get p 1) (get % 1)) (= (get p 0) (get % 0))) asteroids))))

(defn count-asteroids-seen [p slopes asteroids]
  (let [left-grid (filter #(> (get p 0) (get % 0)) asteroids)
        right-grid (filter #(< (get p 0) (get % 0)) asteroids)
        left-asteroids (count (filter #(has-one-point? % (find-c p %) left-grid) slopes))
        right-asteroids (count (filter #(has-one-point? % (find-c p %) right-grid) slopes))
        up-asteroids (if (can-see-asteroid-up? p asteroids) 1 0)
        down-asteroids (if (can-see-asteroid-down? p asteroids) 1 0)]
    (+ left-asteroids right-asteroids up-asteroids down-asteroids)))

(defn solution [filename]
  (let [my-grid (grid 39 39)
        my-map (read-map filename)
        asteroids (get-asteroid-coordinates my-grid my-map)
        asteroids-seen-count (map #(count-asteroids-seen % (find-all-slopes % my-grid) asteroids) asteroids)]
    (reduce max asteroids-seen-count)))

;[26 29] are the coordinates of my bestest asteroid. this is where I have my laser launched

(defn subtract-vectors [a b]
  (vector (- (get a 0) (get b 0)) (- (get a 1) (get b 1))))

(defn get-angle [v]
  (let [atan (Math/atan2 (get v 0) (get v 1))]
    (if (> 0 atan)
      (+ (* 2 Math/PI) atan)
      atan)))

(defn get-all-angles [grid base]
  (reduce conj [] (distinct (sort (map #(get-angle (subtract-vectors % base)) grid)))))

(defn in-angle? [base asteroid angle]
  (= angle (get-angle (subtract-vectors asteroid base))))

(defn distance [a b]
  (let [side (- (get a 0) (get b 0))
        other-side (- (get a 1) (get b 1))]
    (Math/sqrt (+ (* side side) (* other-side other-side)))))

(defn min-distance [base asteroid-one asteroid-two]
  (let [ d1 (distance base asteroid-one)
        d2 (distance base asteroid-two)]
    (if (> d1 d2)
      asteroid-two
      asteroid-one)))

(defn with-loop [base angles asteroids current-angle]
  (loop [b base as angles roids asteroids ca current-angle]
    (do (println "current-angle" ca (count roids))
        (if (= 1 (count roids))
          roids
          (let [angle (get (vec as) ca)
                in-site (filter #(in-angle? base % angle) roids)]
            (do (print in-site base angle)
              (if (empty? in-site)
                (recur b as roids (rem (inc ca) (count as)))
                (let [to-remove (reduce (fn [a b] (min-distance base a b)) in-site)]
                  (recur b as (filter #(not (= to-remove %)) roids) (rem (inc ca) (count as)))))))))))

(defn shoot-asteroids-down [base angles asteroids current-angle l c]
  (println c l (nth angles current-angle) current-angle)
  (if (= 9 c)
    l
    (let [angle (nth angles current-angle)
          in-site (filter #(in-angle? base % angle) asteroids)]
      (if (empty? in-site)
        (shoot-asteroids-down base angles asteroids (rem (inc current-angle) (count angles)) nil c)
        (let [to-remove (reduce (fn [a b] (min-distance base a b)) in-site)]
          (shoot-asteroids-down base angles (filter #(not (= to-remove %)) asteroids) (rem (inc current-angle) (count angles)) to-remove (inc c)))))))



(defn second-part [base grid asteroids]
  (let [angles (sort (get-all-angles grid base))]
    (shoot-asteroids-down base angles asteroids 0 nil 0)))

(defn ttest [filename grid-dimension target base]
  (let [my-grid (grid (nth grid-dimension 0) (nth grid-dimension 0))
        my-map (read-map filename)
        asteroids (filter #(not (= % [26 29])) (get-asteroid-coordinates my-grid my-map))
        angles (sort (get-all-angles my-grid base))]
    (second-part base my-grid asteroids)))
