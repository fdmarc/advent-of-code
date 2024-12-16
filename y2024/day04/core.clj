(ns y2024.day04.core
  (:require
   [clojure.string :as s]
   [clojure.pprint :refer [pprint]]
   [lib :refer (transpose)]))

(defn count-xmas [s]
  (count (filter identity (re-seq #"XMAS" s))))


(defn diagonal-string [lines [row col]]
  (apply str (loop [acc []
                    row row
                    col col]
               (let [ch (get-in lines [row col])]
                 (if (nil? ch)
                   acc
                   (recur (conj acc ch) (inc row) (inc col)))))))


(diagonal-string ["abc" "def" "ghi"] [0 0])

(defn diagonals [lines]

  (let [rows (count lines)
        cols (count (first lines))
        starts (distinct (concat
                          (for [row (range rows)]
                            [row 0])
                          (for [col (range cols)]
                            [0 col])))]

    (pprint {:rows rows
             :cols cols})

    (map (partial diagonal-string lines) starts)))


(diagonals ["abc" "def" "ghi"])

(mapv s/reverse ["abc" "def" "ghi"])

(diagonals (mapv s/reverse ["abc" "def" "ghi"]))



(defn permutate [lines]
  (let [vertical (map (partial apply str) (transpose lines))
        neg-diag (diagonals lines)
        pos-diag (diagonals (mapv s/reverse lines))]

    (concat
     lines

     (map s/reverse lines)
     vertical
     (map s/reverse vertical)

     neg-diag
     (map s/reverse neg-diag)

     pos-diag
     (map s/reverse pos-diag))))

(defn solve [input]
  (let [perms (permutate (s/split-lines input))]

    ;(pprint perms)



    (reduce + (map count-xmas perms))))

  ;(reduce + (mapcat count-xmas ))


;; (solve (slurp "y2024/day04/example.txt"))
;; (solve (slurp "y2024/day04/example2.txt"))
;; (solve (slurp "y2024/day04/input.txt"))



(map vec ["aa" "bb"])

(defn solve-2 [input]

  (let [grid (mapv vec (s/split-lines input))
        results (for [row (range 1 (dec (count grid)))
                      col (range 1 (dec (count (first grid))))
                      :let [ch (get-in grid [row col])]
                      :when (= \A ch)]
                
                  (let [nw (get-in grid [(dec row) (dec col)])
                        ne (get-in grid [(dec row) (inc col)])
                        sw (get-in grid [(inc row) (dec col)])
                        se (get-in grid [(inc row) (inc col)])
                        pos-ch (set [sw ne])
                        neg-ch (set [nw se])
                        debug {:row row
                               :col col
                               :ch ch
                               :others [nw ne sw se]
                               :pos pos-ch
                               :neg neg-ch}]
                
                    ;(pprint debug)
                
                    (if  (= #{\M \S} pos-ch neg-ch)
                      1
                      0)))
        ]
    (print results)
    (reduce + results)
    ))

(solve-2 (slurp "y2024/day04/example3.txt"))

;(map (partial apply str)(transpose ["aa" "bb" "cc"]))


(solve-2 (slurp "y2024/day04/input.txt"))