(ns aoc-solutions.core
  (:gen-class))

(defn my-reduce [my-map my-two-elem-vec]
  (let [key (first my-two-elem-vec)
        values (get my-map key)]
    (assoc my-map key (concat values (list (last my-two-elem-vec))))))

(defn all-orbits-one-node [my-map node]
  (let [orbits (get my-map node)]
    (if (empty? orbits)
      0
      (+ (count orbits) (reduce + (map #(all-orbits-one-node my-map %) orbits))))))


(defn solution-first-part [filename]
  (let [input-vector (map #(split % #"\)") (split (slurp filename) #"\n"))
        my-map (reduce my-reduce {} input-vector)
        nodes (distinct (map #(first %1) my-map))]
    (reduce + (map #(all-orbits-one-node my-map %) nodes))))

(defn depth [my-map node root current-depth]
  (let [filtered (filter #(= node %) root)]
    (if (empty? root)
      -1
      (if (not (empty? filtered))
        current-depth
        (depth my-map node (reduce concat (map #(get my-map %) root)) (inc current-depth))))))

(defn has-elem [l1 elem]
  (not (empty? (filter #(= elem %) l1))))

(defn is-parent? [my-map node1 node2]
  (not (= -1 (depth my-map node1 (list node2) 0))))

(defn intersect [l1 l2]
  (let [union (distinct (concat l1 l2))]
    (filter #(and (has-elem l1 %) (has-elem l2 %)) union)))

(defn find-deepest-parent [my-map node1 node2 root]
  (let [all-nodes (distinct (map #(first %1) my-map))
        node1-parents (filter #(is-parent? my-map node1 %) all-nodes)
        node2-parents (filter #(is-parent? my-map node2 %) all-nodes)
        common (intersect node1-parents node2-parents)]
    (if (empty? common)
      0
      (reduce max (map #(depth my-map % root 0) common)))))

(defn orbital-transfers [my-map node1 node2 parent]
  (let [depth-node1 (depth my-map node1 parent 0)
        depth-node2 (depth my-map node2 parent 0)
        depth-parent (find-deepest-parent my-map node1 node2 parent)]
    (- (+ (dec depth-node1) (dec depth-node2)) (* 2 depth-parent))))
