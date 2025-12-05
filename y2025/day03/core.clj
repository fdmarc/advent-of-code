(ns y2025.day03.core
  (:require [clojure.string :as s]
            [lib :refer [slurp-lines digit->long index-of]]))


(defn parse-banks [path]
  (mapv (comp (partial mapv digit->long) vec) (slurp-lines path)))


(defn joltage [bank]
  (let [tens (last (sort (butlast bank)))
        loc (index-of tens bank)
        ones  (last (sort (drop (inc loc) bank)))]
    (+ ones (* 10 tens))))

(defn solve-1 [path]
  (reduce + (mapv joltage (parse-banks path))))

(solve-1 "y2025/day03/example.txt")
(solve-1 "y2025/day03/input.txt")


(defn count-up [digits]

  (loop [[h & rest] digits
         acc 0]
    (if (nil? h) 
      (do 
        #_(prn "acc" acc)
        acc)
      

        (recur rest
               (+ h (* 10 acc))))))




(defn joltage-2 [bank]
  (loop [bank bank
         idx 12
         acc []]
    (if (zero? idx) (count-up acc)
        (let [len (count bank)
              candidates (take (inc (- len idx)) bank)
              it (apply max candidates)
              pos (index-of it candidates)]
          #_(prn "candidats" candidates "it" it
               "pos" pos "bank" bank)
          (recur
           (drop (inc pos) bank)
           (dec idx)
           (conj acc it))))))


(defn solve-2 [path]
  (reduce + (mapv joltage-2 (parse-banks path))))


(joltage-2 [9 8 7 6 5 4 3 2 1 1 1 1 1 1 1])
(joltage-2 [8 1 8 1 8 1 9 1 1 1 1 2 1 1 1])


(solve-2 "y2025/day03/example.txt")



(solve-2 "y2025/day03/input.txt")



