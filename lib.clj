(ns lib
  (:require [clojure.string :as s]
            [clojure.test :refer [is]]))


(defn- split-by* [pred coll]
  (let [xs (take-while (complement pred) coll)
        ys (drop-while (complement pred) coll)]
    (cond (seq xs)
          [xs ys]

          (seq ys)
          [[(first ys)] (rest ys)]

          :else
          [])))

(defn split-by [pred coll]
  (loop [coll coll
         result []]
    (let [[xs ys] (split-by* pred coll)]
      (cond
        (empty? xs)
        result
        (empty? ys)
        (conj result xs)
        :else
        (recur ys (conj result xs))))))


(is (= [] (split-by zero? [])))
(is (= [[0] [0]] (split-by zero? [0 0])))
(is (= [[1] [0] [2 2] [0] [0] [3 3 3] [0]] (split-by zero? [1 0 2 2 0 0 3 3 3 0])))


(defn parse-ints [s]
  (->>
   (s/split s #"\s+")
   (filter (complement s/blank?))
   (mapv s/trim)
   (mapv parse-long)))

(parse-ints "  12 2   2   2")


(defn transpose [xs]
  (apply mapv vector xs))

(def ^:private nine
  [[1 2 3]
   [4 5 6]
   [7 8 9]])

(is (= [[1 4 7]
        [2 5 8]
        [3 6 9]]
       (transpose nine)))

(is (= nine
       (transpose (transpose nine))))

(defn gcd
  [a b]
  (if (zero? b)
    a
    (recur b, (mod a b))))

(defn lcm
  [a b]
  (/ (* a b) (gcd a b)))

(defn lcmv [& v]
  (reduce lcm v))

(defn flatten-1 [seq-of-seqs]
  (vec (apply concat seq-of-seqs)))


