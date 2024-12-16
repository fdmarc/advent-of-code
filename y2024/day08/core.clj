(ns y2024.day08.core
  (:require
   [clojure.string :as s]
   [clojure.math.combinatorics :as combo]
   [lib :refer (flatten-1)]))


(defn parse-1 [input]
  (let [antenae (filter
                 identity
                 (flatten (map-indexed
                           (fn [row line]
                             (map-indexed
                              (fn [col char]
                                (when-not (#{\. \#} char)
                                  {:loc [row col]
                                   :freq char}))
                              line))
                           (s/split-lines input))))]
    (update-vals
     (group-by :freq antenae)
     #(mapv :loc %))))


(defn antinodes [[max-rows max-cols] [[a b] [x y]]]


  (filter
   (fn [[r c]]
     (and (< -1 r max-rows)
          (< -1 c max-cols)))

   [[(+ a (- a  x))
     (+ b (- b y))]
    [(+ x (- x  a))
     (+ y (- y b))]]))



(defn antinodes-2 [[max-rows max-cols] [[a b] [x y]]]
  (let [row-pos (map (fn [r] (* r (- a x))) (range 0 max-rows))
        col-pos (map (fn [c] (* c (- b y))) (range 0 max-cols))
        row-neg (map - row-pos)
        col-neg (map - col-pos)
        pos (map (fn [r c] [(+ a r) (+ b c)]) row-pos col-pos)
        neg (map (fn [r c] [(+ a r) (+ b c)]) row-neg col-neg)
        candidates (distinct (concat pos neg))]
    (sort (filter
           (fn [[r c]]
             (and (< -1 r max-rows)
                  (< -1 c max-cols)))
           candidates))))


(antinodes-2 [10 8] [[6 1] [6 3]])

(antinodes [20 15] [[1 8] [2 11]])

(defn max-size [input]
  (let [lines (s/split-lines input)
        rows (count lines)
        cols (count (first lines))]
    [rows cols]))

(defn solve-1 [input]
  (let [size (max-size input)
        antenae (parse-1 input)
        nodes (for [[freq locs] antenae]
                (let [pairs (combo/combinations locs 2)
                      nodes (flatten-1 (map (partial antinodes size) pairs))]
                  ;{(format "%s-nodes" freq) nodes}
                  nodes))
        ;result (apply merge antenae nodes)

        dots (set (flatten-1 nodes))
        buf (java.lang.StringBuffer.)]

    (prn "count" (count dots))

    (doseq [row (range (nth size 0))]
      (doseq [col (range (nth size 1))]

        (.append buf (if (contains? dots [row col])
                       "#" ".")))
      (.append buf "\n"))

    (str buf)))

(defn solve-2 [input]
  (let [size (max-size input)
        antenae (parse-1 input)
        nodes (for [[freq locs] antenae]
                (let [pairs (combo/combinations locs 2)
                      nodes (flatten-1 (map (partial antinodes-2 size) pairs))]
                      ;{(format "%s-nodes" freq) nodes}
                  nodes))
            ;result (apply merge antenae nodes)

        dots (set (flatten-1 nodes))
        buf (java.lang.StringBuffer.)]

    (prn "count" (count dots))

    (doseq [row (range (nth size 0))]
      (doseq [col (range (nth size 1))]

        (.append buf (if (contains? dots [row col])
                       "#" ".")))
      (.append buf "\n"))

    (str buf)))


(println (solve-2 (slurp "y2024/day08/example2.txt")))
(println (solve-2 (slurp "y2024/day08/example-t.txt")))
(println (solve-2 (slurp "y2024/day08/input.txt")))

