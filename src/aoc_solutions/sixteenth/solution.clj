(ns aoc-solutions.sixteenth.solution)

(def input (map #(read-string (str %)) (seq "59756772370948995765943195844952640015210703313486295362653878290009098923609769261473534009395188480864325959786470084762607666312503091505466258796062230652769633818282653497853018108281567627899722548602257463608530331299936274116326038606007040084159138769832784921878333830514041948066594667152593945159170816779820264758715101494739244533095696039336070510975612190417391067896410262310835830006544632083421447385542256916141256383813360662952845638955872442636455511906111157861890394133454959320174572270568292972621253460895625862616228998147301670850340831993043617316938748361984714845874270986989103792418940945322846146634931990046966552")))

(defn get-index-phase [index l]
  "l counts how long the phase should be"
  (let [base '(0 1 0 -1)
        expanded (flatten (map #(repeat index %) base))
        phase-length (inc (quot l (count expanded)))
        nextphase (flatten (repeat phase-length expanded))
        shifted (concat (drop 1 nextphase) (take 1 nextphase))
        res (take l shifted)]
    res))

(defn number [phase nums]
  (let [new-nums (map * phase nums)
        summed (reduce + new-nums)]
    (Math/abs (rem summed 10))))

(defn get-all-phases [input-len]
  (vec (map #(get-index-phase % input-len) (range 1 (inc input-len)))))

(defn get-phase-for [index num-index]
  (let [i (* 4 index)
        n-i (rem num-index i)]
    ([0 1 0 -1] (rem (quot (inc n-i) index) 4))))

(defn lcm 
  ([a b] (if (< a b) (lcm a b a) (lcm b a b)))
  ([a b base-a]
     (if (< a b)
       (lcm b a b)
       (if (= 0 (rem a b)) a (lcm (+ a base-a) b base-a)))))

(defn calc-num-at [index nums repetitions]
  (if (= 0 (rem (* (count nums) repetitions) (* 4 index)))
    0
    (let [num-count (count nums)
          period (* 4 index)
          lmc (lcm num-count period)
          delta (rem (* num-count repetitions) lmc)
          rs (inc (quot delta num-count))
          total-length (* num-count rs)
          ns (take delta (drop (- total-length delta) (reduce concat (repeat rs nums))))]
      (Math/abs (rem (reduce + (map-indexed #(* (get-phase-for index %1) %2) ns)) 10)))))

(defn reducer [accumulator nums phases i]
  "not used"
  (concat accumulator (list (number (phases i) nums))))

;(map #(number (index-phases %) nums) indices)

(defn fft [nums phases-left indices repetitions]
  (if (= 0 phases-left)
    (do (println "akakunkaka")
        nums)
    (let [next-nums (map #(calc-num-at (inc %) nums repetitions) indices)]
      (recur next-nums (dec phases-left) indices repetitions))))

(defn solution-first-part [input phase-count]
  (let [l (count input)
        indices (range l)]
    (apply str (take 8 (fft input phase-count indices 1)))))

(defn solution-second-part [input repetitions]
  (let [offset (Integer/parseInt (reduce str (take 7 input)))
        len (* repetitions (count input))
        indices (range len)
        fft-calc (fft input 100 indices repetitions)
        res (take 8 (drop offset fft-calc))]
    (reduce str res)))
