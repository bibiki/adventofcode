;first part of day 4
(defn has-double? [x] (if (< x 10) false (if (= (rem x 10) (rem (quot x 10) 10)) true (has-double? (quot x 10)))))
(defn increasing? [x] (if (< x 10) true (if (< (rem x 10) (rem (quot x 10) 10)) false (increasing? (quot x 10)))))
(count (filter #(and (increasing? %) (has-double? %)) (range 206938 679128)))
