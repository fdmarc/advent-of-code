(ns y2025.day01.core
  (:require [clojure.string :as s]))

(def ^:private directions {"L" -1
                           "R" 1})

(defn- load-turns [path]
  (mapv (fn [it]
          (let [[_ dir cnt] (re-find #"(\w)(\d+)" it)]
            (* (directions dir) (parse-long cnt))))
        (s/split-lines (slurp path))))


(defn solve [path]
  (let [turns (load-turns path)
        places (reductions (fn [at delta]

                             ;(prn (format "At %s delta %s" a b))


                             (mod (+ at delta) 100))

                           50 turns)]

    (get (frequencies places) 0)))


(solve "y2025/day01/example1.txt")
(solve "y2025/day01/input1.txt")

(defn solve-2 [path]
  (let [turns (load-turns path)
        places (loop [[delta & rst] turns
                      at 50
                      result 0]
                 (if (nil? delta) result
                     (let [nxt (+ at delta)
                           nxt-mod (mod nxt 100)
                           [lo hi] (sort [at nxt])
                           places (mapv #(mod % 100) (range lo (inc hi)  ))
                           passes (count (filterv zero? places))
                           ]
                       (prn (format "at %s delta %s next %s [%s] passes %s" at delta nxt nxt-mod passes))


                       (recur rst nxt-mod
                              (+ result passes)))))]

    places))

;(solve-2 "y2025/day01/example1.txt")
(solve-2 "y2025/day01/input1.txt")



(count (filterv zero? (mapv #(mod % 100) (range -998 0))))


(range  -9 (inc 0))