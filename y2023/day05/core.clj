(ns day05.core
  (:require [clojure.string :as s]
            [clojure.test :refer (is are)]
            [lib :refer [parse-ints]]))

(defn parse-seeds-1 [s]
  (parse-ints (second (re-find #"seeds: ([\d ]+)" s))))

(defn parse-seeds-2 [s]
  (let [pairs  (partition  2 (parse-ints (second (re-find #"seeds: ([\d ]+)" s))))]
    (mapcat (fn [[start length]]
           (range start (+ start length))
           
           ) pairs)
    
    
    ))


(parse-seeds-2 "seeds: 79 14 55 13")

(is (= (parse-seeds-1 "seeds: 79 14 55 13")
       [79 14 55 13]))


(defn parse-maps [s]
  (re-seq #"\w+-to-\w+ map:\s+(?:\d+ \d+ \d+\n?)+" s))

(defn parse-range [s]
  (let [[dest source length] (parse-ints s)]
    {:destination dest
     :source source
     :length length}))

(defn parse-map [s]
  (let [[_ a b] (re-find #"(\w+)-to-(\w+).*" s)
        ranges  (mapv parse-range (re-seq #"\d+ \d+ \d+" s))]
    [[(keyword a) (keyword b)] ranges]))

(parse-map "humidity-to-location map:\n60 56 37\n56 93 4")





(defn lookup [haystack needle]
  (loop [[{:keys [destination source length] :as head} & rst] haystack]
    (cond
      (nil? head)
      needle

      (<= source needle (+ source length))
      (+ destination (- needle source))

      :else
      (recur rst))))

(are [seed soil]
     (= soil
        (lookup  [{:destination 50, :source 98, :length 2}
                  {:destination 52, :source 50, :length 48}]
                 seed))
  79 81
  14 14
  55 57
  13 13)


(def steps (partition 2 1
                      [:seed :soil :fertilizer :water :light :temperature :humidity :location]))



(defn find-location [almanac seed]
  (loop [[step & rst] steps
         needle seed]
    (if (nil? step)
      needle
      (recur rst (lookup (get almanac step) needle)))))

(defn parse [parser input]
  (let [seeds (parser input)
        maps (parse-maps input)
        almanac (into {} (mapv parse-map maps))]
    
    (apply min (map (partial find-location almanac)  seeds))))

(parse parse-seeds-1 (slurp "day05/example.txt"))
(parse parse-seeds-1 (slurp "day05/input.txt"))

