(ns day10.core
  (:require [clojure.string :as s]
            [clojure.set :as set]
            [clojure.test :refer [is]]
            [clojure.math.combinatorics :refer [cartesian-product]]
            [lib :refer [parse-ints transpose]]))

(defn find-warps* [galaxy]
  (filter identity (map-indexed (fn [idx row]
                                  (when (every? #{\.} row)
                                    idx))
                                galaxy)))

(defn find-warps [galaxy]
  {:x-warps (find-warps* (transpose galaxy))
   :y-warps (find-warps* galaxy)})

(defn expand* [galaxy]
  (mapcat (fn [row]
            (if (every? #{\.} row)
              [row row]
              [row])) galaxy))

(defn expand [galaxy]
  (-> galaxy
      (expand*)
      (transpose)
      (expand*)
      (transpose)))

(defn print-galaxy [galaxy]
  (doseq [g galaxy]
    (println (apply str g))))



(defn star-map [galaxy]
  (->> galaxy
       (map-indexed (fn [y line]
                      (map-indexed (fn [x ch]
                                     (when (= \# ch)
                                       [x y]))
                                   line)))
       (apply concat)
       (filter identity)))

(defn routes-between [stars]
  (set (for [a stars
             b stars
             :when (not= a b)]
         (sort [a b]))))


(routes-between [1 2 3])

(defn manhatten-distance [[[x1 y1] [x2 y2]]]
  (+ (abs (- x1 x2))
     (abs (- y1 y2))))

(defn warp-distance [boost warps a1 a2]
  (reduce +
          (for [warp warps]
            (if (< (min a1 a2) warp (max a1 a2))
              (dec boost)
              0))))

(is (= 18 (warp-distance 19 [1 10 21] 0 2)))
(is (= 0 (warp-distance 17 [1 10 21] 31 33)))
(is (= 4 (warp-distance 5 [1 10 21] 11 9)))

(defn space-manhatten-distance [boost {:keys [x-warps y-warps]} [[x1 y1] [x2 y2]]]
  (let [x-boost (warp-distance boost x-warps x1 x2)
        y-boost (warp-distance boost y-warps y1 y2)]
    (+ (abs (- x1 x2))
       (abs (- y1 y2))
       x-boost
       y-boost)))

(defn part-1 [input]
  (let [galaxy (map vec (s/split-lines input))
        galaxy (expand galaxy)
        ;_ (print-galaxy galaxy)
        stars (star-map galaxy)
        routes (routes-between stars)]
    (reduce + (map manhatten-distance routes))))


(defn part-2 [boost input]
  (let [galaxy (map vec (s/split-lines input))
        warps (find-warps galaxy)
        stars (star-map galaxy)
        routes (routes-between stars)]
    (reduce + (map (partial space-manhatten-distance boost warps) routes))))

(part-1 (slurp "day11/example1.txt"))

(part-1 (slurp "day11/input.txt"))

(part-2 10 (slurp "day11/example1.txt"))
(part-2 100 (slurp "day11/example1.txt"))
(part-2 1000000 (slurp "day11/input.txt"))