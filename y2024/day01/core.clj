(ns y2024.day01.core
  (:require
   [clojure.string :as s]
   [lib :refer [parse-ints transpose]]))

(defn similarity [haystack needle]
  (let [matches (count (filter (partial = needle) haystack))]
    (* needle matches)))

(defn load-locations [data]
  (let [[a b] (transpose (map parse-ints (s/split-lines data)))
        distances (loop [[x & a-rest] (sort a)
                         [y & b-rest] (sort b)
                         result []]
                    (if (nil? x)
                      result
                      (recur
                       a-rest
                       b-rest
                       (conj result (abs (- x y))))))
        
        similarities (map (partial similarity b) a)
        ]

    #_(reduce + distances)
    (reduce + similarities)
    
    ))

(load-locations (slurp "y2024/day01/example.txt"))
(load-locations (slurp "y2024/day01/input.txt"))