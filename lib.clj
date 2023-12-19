(ns lib
  (:require [clojure.string :as s]))

(defn parse-ints [s]
  (->>
   (s/split s #"\s+")
   (filter (complement s/blank?))
   (mapv s/trim)
   (mapv parse-long)))

(parse-ints "  12 2   2   2")


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
