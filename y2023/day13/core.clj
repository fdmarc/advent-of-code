(ns day13.core
  (:require [clojure.string :as s]
            [clojure.set :as set]
            [clojure.test :refer [is]]
            [clojure.math.combinatorics :refer [cartesian-product]]
            [lib :refer [parse-ints transpose]]))

(defn print-mirror [mirror]
  (doseq [line mirror]
    (prn line))
  (prn))

(defn mirror-size [mirror axis]
  #_(prn "mirror-size" axis mirror)
  (let [halves (map (fn [a b] (= a b))
                    (reverse (take (inc axis) mirror))
                    (nthrest mirror (inc axis)))]
    #_(prn "halve" halves)
    (when (every? true? halves)

      ;(prn "result")
      ;(print-mirror mirror)
      ;(prn axis)
      (inc axis))))

(defn find-axis [mirror]
  (let [axes (->> mirror
                  (partition 2 1)
                  (map-indexed (fn [idx [a b]]
                                 (when (= a b) idx)))
                  (filterv identity))]
    ;(prn "axes" axes)
    ;; Place the cursor on the `t` in filter on the line below
    (->> axes
         (map (partial mirror-size mirror))
         (filter identity)
         (flatten))))

(defn parse-mirror [s]
  (->> s
       (s/split-lines)
       (mapv (partial replace {\# 1 \. 0}))
       (mapv vec)))

(defn part-1 [input]
  (let [mirrors  (map parse-mirror (s/split input #"\n\n"))
        rows  (flatten (map find-axis mirrors))
        cols  (flatten (map find-axis (map transpose mirrors)))]
    {:rows rows
     :cols cols}

    (+ (* 100 (reduce + rows))
       (reduce + cols))))


;(part-1 (slurp "day13/example1.txt"))
;(part-1 (slurp "day13/input.txt"))


(defn flip [b]
  (if (= b 1)
    0
    1))

(defn flip-every-square [mirror]
  (let [w (count (first mirror))
        h (count mirror)]
    (for [y (range h)
          x (range w)]
      (update-in mirror [y x] flip))))


(flip-every-square [[1 1] [0 0]])


(defn part-2* [mirror]
  (let [row (set (find-axis mirror))
        col (set (find-axis (transpose mirror)))

        cleaned-rows (flip-every-square mirror)
        cleaned-cols (flip-every-square (transpose mirror))

        cleaned-row-result (set (mapcat find-axis cleaned-rows))
        cleaned-col-result (set (mapcat find-axis cleaned-cols))]
    {:row (set/difference cleaned-row-result row)
     :col (set/difference cleaned-col-result col)
     :original-row row
     :original-col col
     :cleaned-col-result cleaned-col-result
     :cleaned-row-result cleaned-row-result}))


(defn part-2 [input]
  (let [mirrors  (map parse-mirror (s/split input #"\n\n"))
        results (map part-2* mirrors)
        rows (reduce + (mapcat :row results))
        cols (reduce + (mapcat :col results))]
    (+ (* 100 rows)
       cols)))
    

(part-2 (slurp "day13/example1.txt"))
(part-2 (slurp "day13/input.txt"))
