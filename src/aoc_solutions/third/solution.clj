(ns aoc-solutions.core
  (:gen-class))
;; what is the difference between (rest []) and (next seqable)

;; this is my solution to the second part of the puzzle for day three in Advent of Code
;;wierdly enough, my solution return 7 lines as possible solutions though the problem description says that only one line should be returned.

(defn file-as-seq
  "takes in a filename, and returns a seq with the file's lines as it's elements"
  [filename]
  (line-seq (clojure.java.io/reader filename)))

(defn line-to-map
  "takes in a line like: #1 @ 335,861: 14x10 and returns a map like: {:rowoff 335, :coloff 861, :rowlen 14, :collen 10}"
  [line]
  (zipmap '(:rowoff :coloff :rowlen :collen) (map #(Integer. %) (filter #(not (empty? %)) (next (next (clojure.string/split line #" |\,|x|\:")))))))




(defn corners
  "takes a map like {:rowoff 607, :coloff 196, :rowlen 19, :collen 27} and returns a list like: ({:x 607, :y 196} {:x 607, :y 222} {:x 625, :y 196} {:x 625, :y 222})"
  [area]
  (cons {:x (:rowoff area) :y (:coloff area)}
        (cons {:x (:rowoff area) :y (+ (:coloff area) (:collen area) -1)}
              (cons {:x (+ (:rowoff area) (:rowlen area) -1) :y (:coloff area)}
                    (cons {:x (+ (:rowoff area) (:rowlen area) -1) :y (+ (:coloff area) (:collen area) -1)} '())))))

(defn area-contains-point?
  "takes a map like: {:rowoff 607, :coloff 196, :rowlen 19, :collen 27} to indicate an area and a point like: {:x 607 :y 706} and returns true if area contains point, false otherwise"
  [area point]
  (and
   (<= (:rowoff area) (:x point)) 
   (>= (+ (:rowoff area) (:rowlen area) -1) (:x point))
   (<= (:coloff area) (:y point))
   (>= (+ (:coloff area) (:collen area) -1) (:y point))))

(defn areas-intersect?
  "takes in two maps like: {:rowoff 836, :coloff 17, :rowlen 18, :collen 24} and {:rowoff 836, :coloff 17, :rowlen 18, :collen 24}, and then checks if the two areas intersect. Returns true if the two areas are not the same one and share at least one point."
  [one other]
  (and (not (= one other))
       (or
        (reduce #(or %1 %2) (map #(area-contains-point? other %) (corners one)))
        (reduce #(or %1 %2) (map #(area-contains-point? one %) (corners other))))))

(defn my-test
  []
  (area-contains? (line-to-map "#1 @ 335,861: 14x10") (line-to-map "#1 @ 336,862: 10x5")))

(defn has-overlaps?
  "takes in a map like {:rowoff 836, :coloff 17, :rowlen 18, :collen 24} indicating an area, and a list of similar maps to see if the area intersects any of the area in the list. Returns true, if there is at least one map in the list that intersects the first argument"
  [one others]
  (reduce #(or %1 %2) (map #(areas-intersect? one %) others)))

(defn find-non-overlapping
  "takes in a list of maps indicating areas, and returns a list with the ones that do not overlap with any other areas in the list."
  ([matrices] (find-non-overlapping matrices matrices))
  ([matrices copy-matrices]
   (if (empty? matrices)
     '()
     (if (not (has-overlaps? (first matrices) copy-matrices))
       (cons (first matrices) (find-non-overlapping (rest matrices) copy-matrices))
       (find-non-overlapping (rest matrices) copy-matrices)))))
