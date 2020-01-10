(ns aoc-solutions.core
  (:gen-class))

(defn break-layers [pixels layer-area result]
  (if (empty? pixels)
    result
    (break-layers (drop layer-area pixels) layer-area (into result (list (take layer-area pixels))))))

(defn min-zeros- [one other]
  (let [one-zeros (count (filter #(= "0" %) one))
        other-zeros (count (filter #(= "0" %) other))]
    (if (< one-zeros other-zeros)
      one
      other)))

(defn pixel-color [layers]
  (let [pixel (first layers)]
    (if (or (= 1 (count layers)) (= 0 pixel) (= 1 pixel))
      pixel
      (pixel-color (rest layers)))))

(defn count-occurences [l e]
  (count (filter #(= e %) l)))

(defn solution [filename]
  (let [input (split (trim (slurp "resources/day-eight-2019-input.txt")) #"")
        layers (break-layers input 150 '())
        max-zeros (reduce min-zeros- layers)
        one-count (count-occurences max-zeros "1")
        two-count (count-occurences max-zeros "2")]
    (* one-count two-count)))

(defn rotate-matrix [matrix]
  (map flatten (reduce #(map list %1 %2) matrix)))

(defn print-image [image]
  (if (not (empty? image))
    (do (println (join (take 25 image)))
        (print-image (drop 25 image)))))

(defn solution-second-part [filename]
  (let [image (map pixel-color (rotate-matrix (reverse (break-layers (map #(Integer. %) (split (trim (slurp filename)) #"")) 150 '()))))]
    (print-image image)))
