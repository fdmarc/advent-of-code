(ns day06.core
  (:require [clojure.string :as s]
            [clojure.test :refer (is are)]
            [lib :refer [parse-ints]]))

(defn race [time press]
  (cond
    (= 0 press)
    0

    (= press time)
    0

    :else
    (* press (- time press))))


(race 7 3)

(defn part1* [time record]
  (->> time
       (range)
       (mapv (partial race time))
       (filterv (partial < record))
       (count)))


(defn part1 [time distance]
  (apply * (mapv part1* time distance)))


(part1 [7 15 30]
       [9 40 200])

(part1 [71530]
       [940200])

(part1 [48 98 90 83]
       [390 1103 1112 1360])


(part1 [48989083]
       [390110311121360])