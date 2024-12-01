(ns day17.core
  (:require [clojure.string :as s]
            [clj-async-profiler.core :as prof]
            [clojure.set :as set]
            [clojure.test :refer [is]]
            [clojure.math.combinatorics :refer [cartesian-product]]
            [lib :refer [parse-ints transpose split-by]]))

(defn parse-city [input]
  (->> input
       s/split-lines
       (map-indexed
        (fn [line-idx line]
          (map-indexed
           (fn [col-idx char]
             [[col-idx line-idx] (parse-long (str char))])
           line)))
       (apply concat)
       (into {})))


(defn next-steps [city at path]
  ()
  
  )
(defn depth-first-traversal [city at goal]
  
  ()
  )

(defn find-path [city [x y :as start] [gx gy :as goal]]
  (prn goal)
  (loop [at start
         visited #{} 
         path []
         result {}
         ]
    

    
    
    
    
    ))

(defn extents [city]
  [(->> city keys (map first) (reduce max))
   (->> city keys (map second) (reduce max))])

(defn part-1 [input]
  (let [city (parse-city input)]
    (find-path city [0 0] (extents city))))

(part-1 (slurp "day17/example.txt"))



