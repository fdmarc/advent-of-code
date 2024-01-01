(ns day14.core
  (:require [clojure.string :as s]
            [clj-async-profiler.core :as prof]

            [clojure.set :as set]
            [clojure.test :refer [is]]
            [clojure.math.combinatorics :refer [cartesian-product]]
            [lib :refer [parse-ints transpose split-by]]))


(defn roll-north [col]
  

  ; int x = 0
  ; int y = 0;

  ; while (col[x] != #) x++;
  ; stones(y, x);
  
  ;; for (int x=0; )
  (loop [n (count col)]
    ()
    
    )
  (transient [])
  (let [parts (split-by #{\#} col)]
    (mapcat reverse (map sort parts))))

(defn rotate [cols]
  (apply mapv  vector (reverse cols)))

(defn rotate' [cols]
  (apply mapv  vector (mapv reverse cols)))

(rotate [[1 2]
         [4 3]])

(rotate (rotate [[1 2]
                 [4 3]]))


(rotate' [[1 2]
          [4 3]])

(rotate' (rotate' [[1 2]
                   [4 3]]))

(defn compute-load [col]
  (map-indexed (fn [idx rock]
                 (if (= \O rock)

                   (inc idx)
                   0))
               (reverse col)))


(defn roll [cols]
  (mapv roll-north cols))

(defn print-cols [cols]
  (println)
  (doseq [c (transpose cols)]
    (println (apply str c)))
  (println)

  cols)

(defn cycle-cols [cols]
  (-> cols
      (roll)
      (rotate')
      (roll)
      (rotate')
      (roll)
      (rotate')
      (roll)
      (rotate')))

(defn part-1 [input]
  (let [cols (transpose (s/split-lines input))
        rolled (map roll-north  cols)
        loads (map compute-load rolled)]
    (reduce + (flatten loads))))


(part-1 (slurp "day14/example.txt"))
(part-1 (slurp "day14/input.txt"))

(defn part-2 [input count]
  (let [cols (transpose (s/split-lines input))]
    (print-cols (loop [n 0
            cols cols]
       (if (= n count)
         cols
         (recur (inc n) (cycle-cols cols)))))
    nil
    
    ))

(time (prof/profile (part-2 (slurp "day14/example.txt")
               10000
               )))


(time (part-2 (slurp "day14/example.txt")
                             10000))

; 10,000 => 2868 msec