(ns y2024.day14.core
  (:require [clojure.string :as s]
            [clojure.math :as m]))

;p=0,4 v=3,-3
(def regex #"p=(\d+),(\d+) v=(-?\d+),(-?\d+)")

(defn parse-1 [path]
  (mapv (fn [matches]
          (let [[px py vx vy] (map parse-long (rest matches))]
            {:p [px py]
             :v [vx vy]}))
        (re-seq regex (slurp path))))

(defn simulate [[max-x max-y] seconds {:keys [p v] :as robot}]
  (let [[px py] p
        [vx vy] v]
    (assoc robot
           :end [(mod (+ px (* vx seconds)) max-x)
                 (mod (+ py (* vy seconds)) max-y)])))

(simulate [11 7] 5 {:p [2 4] :v [2 -3]})

(defn to-quadrant [[max-x max-y]]
  (let [split-x (m/floor-div max-x 2)
        split-y (m/floor-div max-y 2)]
    (prn "split" split-x split-y)
    (fn [{:keys [end] :as robot}]
      (let [[px py] end
            quadrant (cond
                       (and (> split-x px)
                            (> split-y py))
                       0

                       (and (< split-x px)
                            (> split-y py))
                       1

                       (and (> split-x px)
                            (< split-y py))
                       2
                       (and (< split-x px)
                            (< split-y py))
                       3)]
        (assoc robot :quadrant quadrant)))))


(defn solve-1 [size seconds path]
  (let [robots (parse-1 path)
        results (mapv
                 (to-quadrant size)
                 (mapv (partial simulate size seconds) robots))]

    (reduce * (vals (dissoc (update-vals (group-by :quadrant results) count) nil)))))


(solve-1 [11 7] 100 "y2024/day14/example.txt")
(solve-1 [101 103] 100 "y2024/day14/input.txt")


(defn simulate-once [[max-x max-y] {:keys [p v] :as robot}]
  (let [[px py] p
        [vx vy] v]
    (assoc robot
           :p [(mod (+ px vx) max-x)
               (mod (+ py vy) max-y)])))

(defn print-robots [[max-x max-y] robots]
  (let [lookup (group-by :p robots)
        sb (StringBuffer.)]
    (dotimes  [x max-x]
      (dotimes  [y max-y]
        (.append sb (if (get lookup [x y])
                      \x \ )))
      (.append sb "\n"))
    (println (str sb))))




(defn to-quadrant-2 [[max-x max-y]]
  (let [split-x (m/floor-div max-x 2)
        split-y (m/floor-div max-y 2)]
    (prn "split" split-x split-y)
    (fn [{:keys [p] :as robot}]
      (let [[px py] p
            quadrant (cond
                       (and (> split-x px)
                            (> split-y py))
                       0

                       (and (< split-x px)
                            (> split-y py))
                       1

                       (and (> split-x px)
                            (< split-y py))
                       2
                       (and (< split-x px)
                            (< split-y py))
                       3)]
        (assoc robot :quadrant quadrant)))))


(defn solve-2 [size times path]
  (let [sim-fn (partial simulate-once size)
        quad-fn (to-quadrant-2 size)
        lowest-score (loop [robots (parse-1 path)
                              iteration 0
                              lowest-score Long/MAX_VALUE
                              result []
                            winner Long/MAX_VALUE
                            ]
                         (if (> iteration times)
                           {:lowest-score lowest-score
                            :iteration iteration
                            :result result
                            :winner winner}
                           (let [quadrants (mapv quad-fn robots)
                                 score (reduce * (vals (dissoc (update-vals (group-by :quadrant quadrants) count) nil)))]
                             (recur (map sim-fn robots)
                                    (inc iteration)
                                    (min lowest-score score)
                                    (if (> lowest-score score)
                                      robots
                                      result)
                                    (if (> lowest-score score)
                                      iteration
                                      winner)
                                    
                                    ))))]
    
    (print-robots size (:result lowest-score))
    lowest-score
    ))
    

(solve-2 [11 7] 20000  "y2024/day14/example.txt")

(comment
  (solve-2 [101 103] 10000 "y2024/day14/input.txt"))