(ns y2024.day15.core
  (:require [clojure.string :as s]
            [clojure.set :refer (map-invert)]
            [lib :refer (character-map)]))

(def char-to-kw {\. nil \# :wall \O :box \@ :robot})
(def kw-to-char (map-invert char-to-kw))

(defn parse-1 [path]
  (let [data (slurp path)
        [map-data move-data] (s/split data #"\n\n")
        path (re-seq #"[<v\^>]" move-data)
        data (character-map map-data)
        parsed (update-vals (:locs data) char-to-kw)
        parsed (into {} (filter second parsed))
        path (mapv {"^" :north "v" :south ">" :east "<" :west} path)
        robot (->> parsed
                   (group-by val)
                   :robot
                   first
                   first)]
    (assoc data
           :robot robot
           :path path
           :locs parsed)))

(defn next-loc [dir [row col]]
  (case dir
    :north [(dec row) col]
    :south [(inc row) col]
    :west [row (dec col)]
    :east [row (inc col)]))

(defn step-1 [{:keys [robot path locs] :as state}]
  (let [dir (first path)
        next-robot (next-loc dir robot)
        #_(prn "attempt to move" robot dir)
        items-to-move
        (loop [acc #{robot}
               pos robot]
          (let [nxt (next-loc dir pos)
                it (get locs nxt)]
            (case it
              nil acc
              :wall #{} ; can't move
              :box (recur
                    (conj acc nxt)
                    nxt))))]
    (if (empty? items-to-move)
      (do
        #_(prn "can't move - no update" robot dir)
        (assoc state :path (rest path)))
      (let [new-locs (update-keys
                      locs
                      (fn [loc]
                        (if (contains? items-to-move loc)
                          (do
                            #_(prn "moving " loc dir)
                            (next-loc dir loc))
                          loc)))]
        #_(prn "moving items" dir items-to-move)
        (assoc state
               :path (rest path)
               :locs new-locs
               :robot next-robot)))))

(defn print-1 [{:keys [locs rows cols] :as state}]
  (let [sb (StringBuffer.)]
    (dotimes [row rows]
      (dotimes [col cols]
        (if-let [it (get locs [row col])]
          (.append sb (kw-to-char it))
          (.append sb ".")))
      (.append sb "\n"))
    (println (str sb))))


(defn cost [[row col]]
  (+ (* 100 row) col))

(defn gps-1 [locs]
  (reduce + (map (comp cost first) (filter (fn [[k v]]
                                    (= :box v)) locs))))

(defn solve-1 [path]
  (let [state (parse-1 path)
        end-state (loop [{:keys [path] :as state} state]
                    #_(print-1 state)
                    (if (empty? path)
                      state
                      (recur (step-1 state))))]
    (-> end-state :locs gps-1)))

(comment
  
  (solve-1 "y2024/day15/input.txt"))



(solve-1 "y2024/day15/example1.txt")
(solve-1 "y2024/day15/example2.txt")
