(ns day09.core
  (:require [clojure.string :as s]
            [clojure.test :refer [is]]
            [lib :refer [parse-ints]]))

(defn diff [[a b]]
  (- b a))

(defn extrapolate [reading]
  (loop [r reading
         result []]
    (if (every? #{0} r)
      (conj result r)
      (recur (mapv diff (partition 2 1 r))
             (conj result r)))))

(defn predict [extrapolation]
  (->> extrapolation
       (map last)
       (reduce +)))


(defn predict-backwards [extrapolation]
  (reduce (fn [acc x]
            (- x acc))
          0
          (reverse (map first extrapolation))))


(defn part-1 [input]
  (let [readings (map parse-ints (s/split-lines input))
        extrapolations (mapv extrapolate readings)]
    (reduce + (map predict extrapolations))))

(defn part-2 [input]
  (let [readings (map parse-ints (s/split-lines input))
        extrapolations (mapv extrapolate readings)]
    (reduce + (map predict-backwards  extrapolations))))


(is (= 114 (part-1 (slurp "day09/example.txt"))))
(part-1 (slurp "day09/input.txt"))


(is (= 2 (part-2 (slurp "day09/example.txt"))))
(is (= 1016 (part-2 (slurp "day09/input.txt"))))