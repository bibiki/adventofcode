

;first puzzle of day 1
(defn solution-first [in]
  (reduce + (map #(- (quot % 3) 2) in)))

;second puzzle of day 1
(defn m [x]
  (let [fuel (- (quot x 3) 2)]
    (if (>= 0 fuel) 0 (+ fuel (m fuel)))))

(defn solution-second [in]
  (reduce + (map m in)))
