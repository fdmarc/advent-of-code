(ns day04.core
  (:require [clojure.string :as s]
            [clojure.set :as set]
            [clojure.math :as math]
            [lib :refer (parse-ints)]))

(defn parse [line]
  (let [[_ card win have] (re-matches #"Card +(\d+):([ \d]+)\|([ \d]+)" line)]
    {:card (parse-long card)
     :win (set (parse-ints win))
     :have (set (parse-ints have))}))

(parse "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53")

(defn points [n]
  (cond
    (= 0 n) 0
    (= 1 n) 1
    :else (long (math/pow 2 (dec n)))))

(points 4)

(defn associate-by [f coll]
  (zipmap (mapv f coll) coll))

(defn score [{:keys [card win have] :as c}]
  (let [matches (set/intersection win have)]
    (assoc c
           :matches matches
           :points (points (count matches))
           :awards (set (range (inc card) (+ (inc card) (count matches)))))))


(defn compute-worth [card others]
  (let [c->card (associate-by :card others)
        wins (select-keys c->card (:awards card))
        points (apply + (mapv :worth (vals wins)))]
    (inc points)))


(compute-worth
 {:card 4, :win #{69 92 41 73 84}, :have #{59 58 54 51 76 5 83 84}, :matches #{84}, :points 1, :awards #{5}}
 [{:card 5, :win #{32 28 83 26 87}, :have #{70 88 22 36 93 12 82 30}, :matches #{}, :points 0, :awards #{} :worth 1}
  {:card 6, :win #{72 31 56 13 18}, :have #{74 77 36 23 35 11 10 67}, :matches #{}, :points 0, :awards #{} :worth 1}])


(defn part2 [scores]
  (loop [[head & tail] (reverse scores)
         result []]
    (if-not (some? head)
      result
      (let [worth (compute-worth head result)
            card-with-worth (assoc head :worth worth)]
        (recur tail (conj result card-with-worth))))))

(defn part1 [scores]
  (apply + (mapv :points scores)))

(defn wins [path]
  (let [data (slurp path)
        lines (s/split-lines data)
        cards (mapv parse lines)
        scores (mapv score cards)]
    (->> scores
         (part2)
         (map :worth)
         (apply +))))

(wins "day04/example.txt")
(wins "day04/input.txt")
