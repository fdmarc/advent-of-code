(ns day16.core
  (:require [clojure.string :as s]
            [clj-async-profiler.core :as prof]
            [clojure.set :as set]
            [clojure.test :refer [is]]
            [clojure.math.combinatorics :refer [cartesian-product]]
            [lib :refer [parse-ints transpose split-by]]))

(defn parse-ch [line-index char-index ch]
  [[char-index line-index] ch])

(defn parse-line [line-index s]
  (map-indexed (partial parse-ch line-index) s))

(defn parse-tiles [input]
  (into {} (apply concat (map-indexed parse-line (s/split-lines input)))))

(defn heading->direction [heading]
  (case heading
    :north [0 -1]
    :south [0  1]
    :east  [1  0]
    :west  [-1  0]))


(defn continue [at heading]
  {:at (map + at (heading->direction heading))
   :heading heading})

(defn next-steps [tile at heading]
  (cond
    (= \. tile)
    [(continue at heading)]

    (and (= \| tile)
         (or (= :north heading)
             (= :south heading)))
    [(continue at heading)]

    (and (= \- tile)
         (or (= :east heading)
             (= :west heading)))
    [(continue at heading)]

    (and (= \- tile)
         (or (= :north heading)
             (= :south heading)))
    [(continue at :east)
     (continue at :west)]

    (and (= \| tile)
         (or (= :east heading)
             (= :west heading)))
    [(continue at :north)
     (continue at :south)]

    (= \\ tile)
    (case heading
      :north #{(continue at :west)}
      :south #{(continue at :east)}
      :east #{(continue at :south)}
      :west #{(continue at :north)})

    (= \/ tile)
    (case heading
      :north #{(continue at :east)}
      :south #{(continue at :west)}
      :east #{(continue at :north)}
      :west #{(continue at :south)})

    :else
    (throw (ex-info "unhandled" {:tile tile :at at :heading heading}))))




(defn illuminate [{:keys [tiles width height]} {:keys [] :as state}]
  (letfn [(valid-state? [{[x y] :at}]
            (and (nat-int? x)
                 (nat-int? y)
                 (> width x)
                 (> height y)))]
    (loop [steps 0
           visited #{}
           frontier [state]]
      #_(prn steps frontier)
      (cond
        (empty? frontier)
        visited

        (= 5000000 steps)
        visited



        :else
        (let [{:keys [at heading] :as where} (first frontier)
              nxt (next-steps (get tiles at) at heading)]
          #_(prn "nxt" nxt)
          (recur
           (inc steps)
           (conj visited where)
           (remove visited (filter valid-state? (distinct (concat (rest frontier) nxt))))))))))


(defn visited-coords [visited-set]
  (set (map :at visited-set)))

(defn print-light [width height visited]
  (dotimes [y height]
    (dotimes [x width]
      (print (if (contains? visited [x y])
               \#
               \.)))
    (println))

  (count visited))


(defn part-1 [input]
  (let [tiles (parse-tiles input)
        width (inc (reduce max (map second (keys tiles))))
        height (inc (reduce max (map first (keys tiles))))]

    (println input)

    (print-light
     width
     height

     (visited-coords
      (illuminate {:width width
                   :height height
                   :tiles tiles}
                  {:at [0 0]
                   :heading :east})))))


(is (= 46 (part-1 (slurp "day16/example.txt"))))
(is (= 6855 (part-1 (slurp "day16/input.txt"))))

(defn part-2 [input]
  (let [tiles (parse-tiles input)
        width (inc (reduce max (map second (keys tiles))))
        height (inc (reduce max (map first (keys tiles))))

        starting-points (concat (map (fn [x y] {:at [x y]
                                                :heading :south}) (range width) (repeat 0))
                                (map (fn [x y] {:at [x y]
                                                :heading :north}) (range width) (repeat (dec height)))
                                (map (fn [x y] {:at [x y]
                                                :heading :east}) (repeat 0) (range height))
                                (map (fn [x y] {:at [x y]
                                                :heading :west}) (repeat (dec width)) (range height)))
        routes (for [s starting-points]
                 (visited-coords
                  (illuminate {:width width
                               :height height
                               :tiles tiles}
                              s)))]
    
    (print-light width height (last (sort-by count routes)))
    
    ))

(part-2 (slurp "day16/example.txt"))
(part-2 (slurp "day16/input.txt"))

