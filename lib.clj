(ns lib
  (:require [clojure.string :as s]
            [clojure.test :refer [is]]))


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
