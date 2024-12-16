(ns y2024.day11.core
  (:require [clojure.string :as s]))

(defn split-stone [stone]
  (let [s (str stone)]
    (mapv (fn [digits]
            (parse-long (apply str digits))) (split-at (/ (count s) 2) s))))

(split-stone 1000)

(defn split-digits? [stone]
  (condp > stone
    10 false
    100 true
    1000 false
    10000 true
    100000 false
    1000000 true
    10000000 false
    100000000 true
    1000000000 false
    10000000000 true
    100000000000 false
    1000000000000 true
    10000000000000 false
    100000000000000 true))

(split-digits? 10000)

(def stone-iterate
  (memoize
   (fn [blinks stone]
     (cond
       (zero? blinks)
       1

       (zero? stone)
       (stone-iterate (dec blinks) 1)

       (split-digits? stone)
       (let [[a b] (split-stone stone)]
         (+   (stone-iterate (dec blinks) a)
              (stone-iterate (dec blinks) b)))

       :else
       (stone-iterate (dec blinks) (* stone 2024))))))

(stone-iterate 1 0)
(stone-iterate 1 1)
(stone-iterate 1 10)
(stone-iterate 1 99)
(stone-iterate 1 999)


(defn solve-1 [blinks stones]
  (reduce + (mapv (partial stone-iterate blinks) stones)))

(defn solve-2 [blinks stones]
  (reduce + (mapv (fn [stone]
                    (prn "starting " stone)
                    (stone-iterate blinks stone)) stones)))

(solve-1 1 [0 1 10 99 999])

(solve-1 6 [125 17])
(solve-1 25 [125 17])

(solve-2 25 [5178527 8525 22 376299 3 69312 0 275])
(solve-2 75 [22])