(ns y2024.day03.core
  (:require [clojure.pprint :refer (pprint)])
  )

(defn solve-1 [s]
  (println s)
  (let  [items (re-seq #"mul\((\d{1,3}),(\d{1,3})\)" s)]
    (reduce + (map (fn [[a b]]
                     (* (parse-long a)
                        (parse-long b))) (map rest items)))))

(solve-1 (slurp "y2024/day03/example.txt"))
(solve-1 (slurp "y2024/day03/input.txt"))

(defn do-and-dont [items]
  (loop [[head & tail] items
         enabled true
         acc []]
    (if (nil? head)
      acc
      (let [[word _ a b] head]
        (case word
          "don't()"
          (recur tail false acc)
          "do()"
          (recur tail true acc)

          (if enabled
            (recur tail true (conj acc (* (parse-long a)
                                          (parse-long b))))
            (recur tail false  acc)))))))

(defn solve-2 [s]
  (println s)
  (let  [items (re-seq #"(do\(\))|don\'t\(\)|mul\((\d{1,3}),(\d{1,3})\)" s)]
    (pprint items)
    (reduce + (do-and-dont items))))


(solve-2 (slurp "y2024/day03/example2.txt"))
(solve-2 (slurp "y2024/day03/input.txt"))