(ns y2025.day02.core
  (:require [clojure.string :as s]))


(defn- parse [path]
  (let [pairs (mapv #(s/split % #"-") (s/split (slurp path) #","))]
    (mapv #(mapv parse-long %) pairs)))


(def example "y2025/day02/example.txt")
(def input "y2025/day02/input.txt")

(parse example)


(defn- ids-in-range [[lo hi]]
  (mapv str (range lo (inc hi))))


(partition 2 "1212")


(defn valid-id? [id]
  (let [len (count id)]
    (if (odd? len) true
        (let [half (/ (count id) 2)
              a (subs id 0 half)
              b (subs id half)]
          (not= a b)))))

(valid-id? "118851188")


(defn invalid-id? [id]
  (let [len (count id)
        half (/ len 2)]
    (loop [n 1]
      (if
       (> n half)
        false
        (let [parts (partitionv-all n id)]
        ;  (prn n parts)
          (if (apply = parts)
            true
            (recur (inc n))))))))


(invalid-id? "2121212118")



(reduce + (mapv parse-long (filterv  valid-id? (ids-in-range [11 22]))))


(defn- solve-1 [path]
  (->> path
       parse
       (mapcat ids-in-range)
       (remove valid-id?)
       (mapv parse-long)
       (reduce +)))

(defn- solve-2 [path]
  (->> path
       parse
       (mapcat ids-in-range)
       (filter invalid-id?)
       (mapv parse-long)
       ;(mapv prn)
       (reduce +)))


(solve-1 example)
(solve-1 input)

(solve-2 example)
(solve-2 input)