(ns y2024.day12.core
  (:require [clojure.string :as s]
            [clojure.pprint :refer [pprint]]
            [lib :refer [flatten-1]]))

(defn parse-1 [path]
  (into {}
        (flatten-1
         (map-indexed
          (fn [row line]
            (map-indexed
             (fn [col plant]
               [[row col] plant])
             line))
          (s/split-lines (slurp path))))))

(defn adjacent [[row col]]
  [[(inc row) col]
   [(dec row) col]
   [row (inc col)]
   [row (dec col)]])


(defn same-plant [plot-map plant loc]
  (for [adj (adjacent loc)
        :let [p (get plot-map adj)]
        :when (= p plant)]
    adj))

(defn find-region [plot-map loc]
  (let [plant (get plot-map loc)]
    (loop [plants #{}
           [current & frontier] [loc]]
      (if (nil? current)
        plants
        (let [nbors (same-plant plot-map plant current)]
          (recur
           (conj plants current)
           (doall
            (remove plants (concat frontier nbors)))))))))




(defn count-fences [region loc]
  (reduce + (for [adj (adjacent loc)]
              (if (contains? region adj)
                0
                1))))



(defn adjacent-2 [[row col]]
  [{:name :south
    :test [(inc row) col]
    :nbor [row (dec col)]
    :nbor-test [(inc row) (dec col)]}

   {:name :west
    :test [row (dec col)]
    :nbor [(dec row) col]
    :nbor-test [(dec row) (dec col)]}

   {:name :north
    :test [(dec row) col]
    :nbor [row (inc col)]
    :nbor-test [(dec row) (inc col)]}

   {:name :east
    :test [row (inc col)]
    :nbor [(inc row) col]
    :nbor-test [(inc row) (inc col)]}])

(defn count-fences-2 [region loc]
  (reduce + (for [{:keys [name test nbor nbor-test]} (adjacent-2 loc)]
              (cond
                (contains? region test)
                0

                (and (contains? region nbor)
                     (not (contains? region nbor-test)))
                0

                :else
                1))))

(defn compute-boundary [counter-fn region]
  (loop [[plant & others] (vec region)
         result 0]
    (if (nil? plant)
      result
      (recur others
             (+ result
                (counter-fn region plant))))))


(defn find-regions-1 [path]
  (println)
  (let [plot-map (parse-1 path)
        candidates (keys plot-map)]
    (loop [candidates candidates
           regions []]
      (if (empty? candidates)
        regions
        (let [candidate (first candidates)
              region (find-region plot-map candidate)]
          (recur
           (remove region candidates)
           (let [plant (get plot-map candidate)
                 boundary (compute-boundary count-fences region)
                 boundary-2 (compute-boundary count-fences-2 region)
                 area (count region)
                 cost (* boundary area)
                 cost-2 (* boundary-2 area)]
             (println
              (format "%s: %s b1: %s b2: %s a: %s cost-1: %s cost-2: %s" path plant boundary boundary-2 area cost cost-2))
             (conj regions {:plant plant
                            ;:region region
                            :boundary boundary
                            :boundary-2 boundary-2
                            :area area
                            :cost cost
                            :cost-2 cost-2}))))))))


(defn solve-1 [path]
  (let [regions (find-regions-1 path)
        cost-1 (reduce + (map :cost regions))
        cost-2 (reduce + (map :cost-2 regions))]
    
        
    {:cost-1 cost-1
     :cost-2 cost-2}))

(solve-1 "y2024/day12/example1.txt")
(solve-1 "y2024/day12/example2.txt")
(solve-1 "y2024/day12/example3.txt")
(solve-1 "y2024/day12/example4.txt")
(comment
  (solve-1 "y2024/day12/input.txt"))