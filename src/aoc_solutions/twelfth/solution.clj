(ns aoc-solutions.core)

(defn- axis-gravity [axis one-moon other-moon]
  (- (compare (axis one-moon) (axis other-moon))))

(defn delta-gravity [one-moon other-moon]
  {:x (axis-gravity :x one-moon other-moon)
   :y (axis-gravity :y one-moon other-moon)
   :z (axis-gravity :z one-moon other-moon)})

(defn sum-maps [m1 m2]
  {:x (+ (:x m1) (:x m2))
   :y (+ (:y m1) (:y m2))
   :z (+ (:z m1) (:z m2))})

(defn diff-maps [m1 m2]
  {:x (- (:x m1) (:x m2))
   :y (- (:y m1) (:y m2))
   :z (- (:z m1) (:z m2))})

(defn calculate-velocity [moon moons]
  (reduce sum-maps {:x 0 :y 0 :z 0} (map #(delta-gravity moon %1) moons)))

(defn velocity [moon velocity]
  (sum-maps moon velocity))

(defn energy [moon]
  (+ (Math/abs (:x moon)) (Math/abs (:y moon)) (Math/abs (:z moon))))

(defn distance-center [moon]
  (+ (* (:x moon) (:x moon))
     (* (:y moon) (:y moon))
     (* (:z moon) (:z moon))))

(defn to-dec 
  ([bin] (to-dec bin 0))
  ([bin rez]
   (if (empty? bin)
     rez
     (to-dec (drop 1 bin) (+ (* 2 rez) (first bin))))))

(defn quadrant [moon]
 (let [l (list (Math/signum (* 1.0 (:x moon))) (Math/signum (* 1.0 (:y moon))) (Math/signum (* 1.0 (:z moon))))
        p (map #(if (= -1.0 %) 0 1) l)]
    (to-dec p)))

(defn moon-hash 
  ([moon] (let [d (distance-center moon)
         q (quadrant moon)
         d-digits (Math/ceil (Math/log10 d))
         d-hash (Integer/toHexString (+ d (* q (int (Math/pow 10 d-digits)))))
]
            d-hash))
  ([moon velocity]
   (str (moon-hash moon) (moon-hash velocity))
   ))

(defn h [moon]
  (let [x (Math/abs (:x moon))
        y (Math/abs (:y moon))
        z (Math/abs (:z moon))
        q (quadrant moon)
        xyz (str x y z)
        xyz-num (Long/parseLong xyz)
        r (+ (* 10 xyz-num) q)]
    r))

(defn calc [moons velocities steps]
  (if (= 1000 steps)
    (reduce + (map * (map energy moons) (map energy velocities)))
       (let [new-velocities (map #(calculate-velocity % moons) moons)
             v (map sum-maps velocities new-velocities)
             m (map sum-maps moons v)]
         (recur m v (inc steps)))))

(defn solution []
  (let [moons [{:x 1 :y -4 :z 3}
               {:x -14 :y 9 :z -4}
               {:x -4 :y -6 :z 7}
               {:x 6 :y -9 :z -11}]

        velocities [{:x 0 :y 0 :z 0} {:x 0 :y 0 :z 0} {:x 0 :y 0 :z 0} {:x 0 :y 0 :z 0}]]
    (calc moons velocities 0)))

(defn digit-count [n]
  (if (> 10 n) 1 (Math/ceil (Math/log10 n))))

(defn my-h [moon quadrant]
  (let [ps (for [[k v] moon] (Math/abs v))
        powers (map #(inc (digit-count %)) ps)
        l 1
        s (+ l (last powers))
        f (inc (reduce + (drop 1 powers)))
        ppp (map #(long (Math/pow 10 %)) (list f s l))]
    (+ quadrant (reduce + (map * ps ppp)))))

(defn cc [a b]
  (let [c (digit-count b)
        d (Math/pow 10 (inc c))]
    (+ (* a d) b)))

(defn test-eq [m-current v-current m-start v-start test-step step]
  (if (= step test-step)
    false
    (if (and (= m-current m-start) (= v-current v-start))
      true
      (let [n-v (map #(calculate-velocity % m-current) m-current)
            v-s (map sum-maps v-current n-v)
            m-s (map sum-maps m-current v-s)]
        (recur m-s v-s m-start v-start (inc test-step) step
         )))))

(defn second-part-calc [init-m init-v moons velocities step]
;  (println step)
;  (println moon-positions)
  (let [new-velocities (map #(calculate-velocity % moons) moons)
        v (map sum-maps velocities new-velocities)
        m (map sum-maps moons v)]
    (if (test-eq init-m init-v moons velocities 0 step)
      step
      (recur init-m init-v m v (inc step)))))

(def r [{:x 1 :y -4 :z 3}
               {:x -14 :y 9 :z -4}
               {:x -4 :y -6 :z 7}
               {:x 6 :y -9 :z -11}])

(def t [{:x -1 :y 0 :z 2}
        {:x 2 :y -10 :z -7}
        {:x 4 :y -8 :z 8}
        {:x 3 :y 5 :z -1}])



(defn solution-2 []
  (let [moons r
        velocities [{:x 0 :y 0 :z 0} {:x 0 :y 0 :z 0} {:x 0 :y 0 :z 0} {:x 0 :y 0 :z 0}]]
    (second-part-calc moons velocities moons velocities 0)))

;<x=-8, y=-10, z=0>
;<x=5, y=5, z=10>
;<x=2, y=-7, z=3>
;<x=9, y=-8, z=-3>


