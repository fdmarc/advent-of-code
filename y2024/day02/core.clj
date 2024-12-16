(ns y2024.day02.core
  (:require [clojure.string :as s]
            [lib :refer [parse-ints transpose]]))

(defn safety-report2 [levels]
  (let [ordered (distinct (sort levels))]

    (or
     (= levels ordered)
     (= levels (reverse ordered)))))

(defn biggest-change [levels]

  (let [d (partition 2 1 levels)

        result (map (fn [[a b]]
                      (abs (- a b)))
                    d)

        biggest (apply max result)]

    ;(println levels)
    ;(println biggest)
    biggest))

(defn safety-report [input]
  (let [changes (map biggest-change (filter safety-report2 (map parse-ints (s/split-lines input))))]
    ;(println changes)
    (count (filter (partial >= 3) changes))))

(defn are-any-safe? [levels]
  (let [changes (map biggest-change (filter safety-report2 levels))]
      ;(println changes)
     (pos-int? (count (filter (partial >= 3) changes)))))


(are-any-safe? [[1 2] [1 7] [ 1 2 3 4]])
  

(safety-report (slurp "y2024/day02/example.txt"))
(safety-report (slurp "y2024/day02/input.txt"))

(defn permutations [levels]
  (let [goodies (loop [[head & tail] levels
                       prev []
                       acc []]

                  (if (nil? head)

                    acc
                    (recur tail
                           (conj prev head)
                           (conj acc (concat prev tail)))))]

    (concat [levels] goodies)))



(defn safety-report-part-2 [input]
  (let [all-the-things (map permutations input)
        results (map are-any-safe? all-the-things)]
    results))

(safety-report-part-2 (map parse-ints (s/split-lines (slurp "y2024/day02/example.txt"))))
(count (filter identity (safety-report-part-2 (map parse-ints (s/split-lines (slurp "y2024/day02/input.txt"))))))
        

(permutations [1 2 3])
