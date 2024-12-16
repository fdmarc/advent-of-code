(ns y2024.day10.core
  (:require [clojure.string :as s]
            [lib :refer [flatten-1]]))

(defn parse-1 [path]
  (let [lines (s/split-lines (slurp path))]
    (->> lines
         (map-indexed
          (fn [row line]
            (map-indexed
             (fn [col height]
               [[row col] {:height (Character/getNumericValue height)}])
             line)))
         flatten-1
         (into {}))))

(defn adjacent [[row col]]
  [[(inc row) col]
   [(dec row) col]
   [row (inc col)]
   [row (dec col)]])


(defn to-graph [top-map]
  (into {} (for [[[x y] {:keys [height] :as node}] top-map]
             [[x y] {:loc [x y]
                     :height height
                     :neighbours (into [] (filter identity
                                                  (for [it (adjacent [x y])]

                                                    (when-let [candidate (get top-map it)]
                                                      (when (= (:height candidate) (inc height))
                                                        it)))))}])))


(defn search-1-impl [graph path {:keys [neighbours height loc] :as node}]
  #_(prn "visit" visited  node)
  (if (= 9 height)
    [{:loc loc :path path}]
    (flatten-1 (filter identity (for [n neighbours]
                          (search-1-impl
                           graph
                           (conj path loc)
                           (get graph n)))))))

(defn search-1 [graph start]
  (count (set (map :loc (search-1-impl graph [] start)))))
    

(defn solve-1 [top-map]
  (let [graph (to-graph top-map)]
    (reduce + (for [[[x y] node] graph
           :when (-> node :height zero?)]
       (search-1 graph node)))))


(solve-1 (parse-1 "y2024/day10/example1.txt"))
(solve-1 (parse-1 "y2024/day10/example2.txt"))
(solve-1 (parse-1 "y2024/day10/input.txt"))


(defn search-2 [graph start]
  (count (map :path (search-1-impl graph [] start))))


(defn solve-2 [top-map]
  (let [graph (to-graph top-map)]
    (reduce + (for [[[x y] node] graph
                    :when (-> node :height zero?)]
                (search-2 graph node)))))

(solve-2 (parse-1 "y2024/day10/example2.txt"))
(solve-2 (parse-1 "y2024/day10/example3.txt"))
(solve-2 (parse-1 "y2024/day10/example4.txt"))
(solve-2 (parse-1 "y2024/day10/example5.txt"))
(solve-2 (parse-1 "y2024/day10/input.txt"))