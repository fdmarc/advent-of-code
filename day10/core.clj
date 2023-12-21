(ns day10.core
  (:require [clojure.string :as s]
            [clojure.set :as set]
            [clojure.test :refer [is]]
            [lib :refer [parse-ints]]))



(defn parse-map [input]
  (into {}
        (apply concat
               (map-indexed
                (fn [y line]
                  (map-indexed (fn [x tile]
                                 [[x y] tile])
                               line)) (s/split-lines input)))))

(defn find-start [nodes]
  (get (set/map-invert nodes) \S))


(defn go [direction [x y]]
  (case direction
    :up    [x (dec y)]
    :down  [x (inc y)]
    :left  [(dec x) y]
    :right [(inc x) y]))

(defn step1 [nodes [x y]]
  (let [up    (get nodes [x (dec y)])
        down  (get nodes [x (inc y)])
        left  (get nodes [(dec x) y])
        right (get nodes [(inc x) y])]
    (prn nodes)
    (prn up down left right)
    (cond
      (#{\7 \| \F} up)
      :up

      (#{\J \| \L} down)
      :down

      (#{\F \- \L} left)
      :left

      (#{\7 \- \J} right)
      :right)))


(def follow



  {[:up \S] :up
   [:down \S]  :down
   [:left \S] :left
   [:right \S] :right

   [:up \7] :left
   [:up \|] :up
   [:up \F] :right

   [:down \J] :left
   [:down \|] :down
   [:down \L] :right

   [:left \L] :up
   [:left \-] :left
   [:left \F] :down

   [:right \J] :up
   [:right \-] :right
   [:right \7] :down})


(defn find-path [nodes start]
  (prn "starting at" start)
  (loop [direction (step1 nodes start)
         at start
         path []
         count 0]
    (let [node (get nodes at)
          next-dir  (get follow [direction node])]
      ;(printf "step=%s at=%s node=%s direction=%s\n" count at node direction)
      (cond (and (= \S node)
                 (pos? count))
            path

            (= 20000 count)
            :overflow

            :else
            (recur next-dir
                   (go next-dir at)
                   (conj path at)
                   (inc count))))))



(def pretty-lookup*
  {\S \u25A3
   \| \u2551
   \- \u2550
   \L \u255A
   \7 \u2557
   \F \u2554
   \J \u255D})

(defn pretty-lookup [ch]
  (get pretty-lookup* ch \space))

(defn pretty-print [nodes path]
  (let [pset (set path)
        xx (inc (reduce max (map first (keys nodes))))
        yy (inc (reduce max (map last  (keys nodes))))]
    (dotimes [y yy]
      (dotimes [x xx]
        (if (contains? pset [x y])
          (let [ch (pretty-lookup (get nodes [x y]))]
            (print ch))
          (print \space)
          ))
      (println))))



(defn part-1 [input]
  (println input)
  (let [nodes (parse-map input)
        start (find-start nodes)
        path (find-path nodes start)]
    (pretty-print nodes path)
    [(/ (count path) 2) path]))


(part-1 (slurp "day10/example2.txt"))
(part-1 (slurp "day10/example2.txt"))
(part-1 (slurp "day10/example3.txt"))
(part-1 (slurp "day10/example4.txt"))
(part-1 (slurp "day10/input.txt"))

