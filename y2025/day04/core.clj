(ns y2025.day04.core
  (:require [clojure.string :as s]
            [clojure.set :as set]
            [lib :refer [slurp-lines digit->long index-of character-map]]))

(def example "y2025/day04/example.txt")
(def input  "y2025/day04/input.txt")


(def neighbours
  [[-1 -1] [0 -1] [1 -1]
   [-1  0]         [1  0]
   [-1  1] [0  1] [1  1]])

(defn accessible [rolls]
  (into #{} (filter identity
                    (for [[i j] rolls]
                      (let [adjacent (reduce + (for [[x y] neighbours]
                                                 (if (contains? rolls [(+ i x) (+ j y)])
                                                   1 0)))]
                        (when (< adjacent 4)
                          [i j]))))))

(defn solve-1 [path]
  (let [{:keys [locs]} (character-map (slurp path))
        rolls (set (keys (filter (comp #{\@} last) locs)))]
    (accessible rolls)))



(count (solve-1 example))
(count (solve-1 input))


(defn solve-2 [path]
  (let [{:keys [locs]} (character-map (slurp path))
        rolls (set (keys (filter (comp #{\@} last) locs)))]

    (loop [rolls rolls
           total 0]
      (let [acc (accessible rolls)]
        (if (zero? (count acc))
                   total
          (recur 
           (set/difference rolls acc)
           (+ total (count acc))))))))


(solve-2 example)
(solve-2 input)
           
           

