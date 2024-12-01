(ns day18.core
  (:require [clojure.string :as s]
            [clj-async-profiler.core :as prof]
            [clojure.set :as set]
            [clojure.test :refer [is]]
            [clojure.math.combinatorics :refer [cartesian-product]]
            [lib :refer [parse-ints transpose split-by]]))


(defn parse-instruction [line]
  (let [[_ direction meters color] (re-find #"(\w) (\d+) \(#(\w{6})\)" line)]
    {:direction direction
     :meters (parse-long meters)
     :color color}))

(defn parse [input]
  (mapv parse-instruction (s/split-lines input)))


(defn dig [instructions]
  (second (reduce (fn [[[x y] lagoon] {:keys [direction meters colors]}]
             (prn x y)

             (let [places (for [m (range meters)]
                            (case direction
                              "R" [(+ x m) y]
                              "L" [(- x m) y]
                              "U" [x (- y m)]
                              "D" [x (+ y m)]))]
               [(last places)

                (reduce (fn [lagoon pos]
                          (assoc lagoon pos \#))
                        lagoon
                        places)]))
           [[0 0] {}]
           instructions)))


(defn print-map [lagoon]
  (prn lagoon)
  (let [mx (apply max (mapv first (keys lagoon)))
        my (apply max (mapv second (keys lagoon)))]

    (dotimes [y my]
      (dotimes[x mx]
        (if (get lagoon [x y])
          (print "#")
          (print ".")))
      (println))))
(defn part-1 [input]
  (print-map (dig (parse input))))


(part-1 (slurp "day18/example.txt"))
