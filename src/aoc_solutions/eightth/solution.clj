(ns aoc-solutions.core
  (:gen-class))


(defn second-part-day-four [input]
  (count (filter my-filter input)))

(defn my-filter [x]
  (and (increasing? x) (has-isolated-ones (to-zeros-ones (cpuples x)))))

(defn couples [x]
  (if (< x 100)
    (vector x)
    (conj (couples (quot x 10)) (rem x 100))))

(defn- increasing? [x] (if (< x 10) true (if (< (rem x 10) (rem (quot x 10) 10)) false (increasing? (quot x 10)))))

(defn first-part-day-four []
  (count (filter #(and (increasing? %) (has-double? %)) (range 206938 679128))))

(defn- to-zeros-ones [x]
  (concat '(0) (map #(if (= 0 (rem % 11)) 1 0) x) '(0)))

(defn- has-isolated-one [x];I broke this one
  (if (< (count x) 3)
    false
    (let [three (take 3 x)]
      (if (= '(0 1 0) three)
        true
        (has-isolated-one (rest x))))))

(defn has-double? [x]
  (if (< x 1000)
    false
    (let [four (rem x 10000)
          f-two (quot four 100)
          s-two (mod (quot four 10) 100)
          t-two (mod four 100)
          couples (list f-two s-two t-two)
          has-double (= 1 (count (filter #(= 0 (rem % 11)) couples)))]
      (if has-double true (has-double? (quot x 10))))))

;;day3 of 2019

(defn get-index [l e]
  (count (take-while #(not (= % e)) l)))

(defn my-solution-second [scheme-l1 scheme-l2]
  (let [grid1 (build-grid scheme-l1 '((0 0)))
        grid2 (build-grid scheme-l2 '((0 0)))
        intersections (list-intersection (rest grid1) (rest grid2) '());to skip (0 0)
        steps-l1 (map #(get-index grid1 %) intersections)
        steps-l2 (map #(get-index grid2 %) intersections)
        combined (map vector steps-l1 steps-l2)
        sums (map #(+ (get % 0) (get % 1)) combined)]
    (print (distinct intersections))
    (reduce min sums)))

(defn build-grid [schemes-list grid]
  (if (empty? schemes-list)
    grid
    (build-grid (rest schemes-list) (concat grid (wire (last grid) (first schemes-list))))))

(defn list-contains-elem? [l e]
  (if (empty? l)
    false
    (not (empty? (filter #(= e %) l)))))

(defn list-intersection [l1 l2 rez]
  (if (empty? l2)
    rez
    (let [first100 (take 100 l2)
          drop100 (drop 100 l2)
          common100 (filter #(list-contains-elem? l1 %) first100)]
      (list-intersection l1 drop100 (concat rez common100)))))

(defn my-solution [scheme-l1 scheme-l2]
  (let [grid1 (build-grid scheme-l1 '((0 0)))
        grid2 (build-grid scheme-l2 '((0 0)))
        intersections (list-intersection (rest grid1) (rest grid2) '());to skip (0 0)
        distances (map #(+ (abs (first %)) (abs (last %))) intersections)]
    (reduce min distances)))

(defn get-op [letter]
  (case letter
    "U" up
    "D" down
    "L" left
    "R" right));no default

(defn wire [from scheme]
  (let [op (subs scheme 0 1)
        rng (range 1 (+ 1 (Integer. (subs scheme 1))))]
    (map #((get-op op) from %) rng)))


(defn- left [from steps]
  (list (- (first from) steps) (last from)))

(defn- right [from steps]
  (list (+ (first from) steps) (last from)))

(defn- up [from steps]
  (list (first from) (+ (last from) steps)))

(defn- down [from steps]
  (list (first from) (- (last from) steps)))

(defn abs [x] (max x (- x)))
;;day two of 2019
(defn calc [op opnd1 opnd2]
  (if (= 1 op)
    (+ opnd1 opnd2)
    (* opnd1 opnd2)))

(defn s [x round]
  (let [four (take 4 (drop (* round 4) x))
        op (first four)]
    (if (= 99 op)
      (first x)
      (let [operands (take 2 (drop 1 four))
            calculation (calc op (get x (first operands)) (get x (last operands)))
            update-x (assoc x (last four) calculation)]
        (s update-x (inc round))))))

;funny because switching around the if-else branches of my second if results in StackOverflow
(defn s-second [x noun verb]
  (let [update-x (assoc (assoc x 1 noun) 2 verb)
        rez (s update-x 0)]
    (if (= rez 19690720)
      (+ (* 100 noun) verb)
      (if (= noun 99)
        (s-second x 0 (inc verb))
        (s-second x (inc noun) verb)))))
