(ns aoc-solutions.core
  (:gen-class))


(defn file-as-seq
  "takes in a filename, and returns a seq with the file's lines as it's elements"
  [filename]
  (line-seq (clojure.java.io/reader filename)))


(defn get-guard
  "takes in a line like: [1518-03-30 23:54] Guard #2039 begins shift, and returns #2039"
  [line]
  (get (clojure.string/split line #" ") 3))

(defn get-time
  [line]
  (let [t (get (clojure.string/split line #" |]") 1)]
    (if (clojure.string/starts-with? t "00")
      t
      "00:00")))

(defn get-minute
  [time]
  (Integer. (get (clojure.string/split time #":") 1)))


(defn inc-range
  "v is a vector of 60 zeros, initially, modeling minutes 0 through 59,
  start and end mark minutes when guard falls asleep and when he wakes up"
  [v start end]
  (map-indexed #(if (or (< %1 start) (<= end %1)) %2 (inc %2)) v))


(defn solution
  ([lines] (solution (rest lines) (get-guard (first lines)) {}))
  ([lines current-guard guard-times]
   (if (empty? lines) guard-times
       (let [l (first lines)]
         (if (clojure.string/includes? l "Guard")
           (solution (rest lines) (get-guard l) guard-times)
           (solution (rest lines) current-guard guard-times (get-time l))))))
  ([lines current-guard guard-times start]
   (solution (rest lines) current-guard guard-times start (get-time (first lines))))
  ([lines current-guard guard-times start end]
   (let [sleeps (get-minute start)
         wakes (get-minute end)
         sleeping (inc-range (vec (repeat 60 0)) sleeps wakes)
         guard-time (get guard-times current-guard)]
     (if (= nil guard-time)
       (solution lines current-guard (assoc guard-times current-guard sleeping))
       (solution lines current-guard
                 (assoc guard-times current-guard (map + (get guard-times current-guard) sleeping)))))))

(defn index-to-elem
  [one-vector]
  (map-indexed #(vector %1 %2) one-vector))

(defn max-vector-second-elem
  [one two]
  (if (> (get (vec one) 1) (get (vec two) 1)) one two))

(defn calc-minutes-asleep
  "takes as input a string-to-vector map, and returns a string-to-integer map. keys continue the same, values in the result are sums of vector elements for each key."
  [guard-times]
  (reduce-kv #(assoc %1 %2 (reduce + %3)) {} guard-times))

(defn find-max-value
  "takes in a string-to-vector map k:v, and returns a map with the same keys onto two-elements vectors [a b]. k:[a b] - a is the index of b in the vector v in the initial map for key k.
  This reduces each value, does not compare two values in the map"
  [guard-times]
  (reduce-kv #(assoc %1 %2 (reduce max-vector-second-elem (index-to-elem %3))) {} guard-times))


; final part should find max in a k:[a b] map (string-to-vector) comparing by second element in the vectors
