(ns y2024.day06.core
  (:require [clojure.string :as s]))

(defn map-size [input]
  (let [lines (s/split-lines input)]
    [(count lines) (count (first lines))]))

(defn start-point [input]
  (first (filter identity (map-indexed
                           (fn [row line]
                             (when-let [col (s/index-of line "^")]
                               [row col]))
                           (s/split-lines input)))))

(defn find-walls [input]
  (into #{}
        (filter identity
                (apply
                 concat
                 (map-indexed (fn [row line]
                                (map-indexed
                                 (fn [col char]
                                   (when (= \# char)
                                     [row col]))
                                 line))
                              (s/split-lines input))))))


; east 
(defn- next-step [walls direction row col]
  (let [next-dir (case direction
                   :north :east
                   :east :south
                   :south :west
                   :west :north)

        [nrow ncol] (case direction
                      :north [(dec row) col]
                      :east [row (inc col)]
                      :south [(inc row) col]
                      :west [row (dec col)])]

    (if (get walls [nrow ncol])
      (next-step walls next-dir row col)
      {:next-dir direction
       :next-pos [nrow ncol]})))


(next-step #{[7 8] [8 7]} :east 7 7)


(defn debug [walls row-max col-max path]
  (let [sb (java.lang.StringBuilder.)]
    (println)
    (doseq [row (range row-max)]
      (doseq [col (range col-max)]
        (cond
          (get walls [row col])
          (.append sb "#")

          (contains? (set path) [row col])
          (.append sb "X")


          :else
          (.append sb ".")))
      (.append sb "\n"))
    (println (str sb))
    (println)))

(defn do-walk [walls start [row-max col-max]]
  (loop [[row col] start
         direction :north
         visited [start]
         depth 1]

    (let [{:keys [next-pos next-dir]} (next-step walls direction row col)]
      ;(debug walls row-max col-max visited)

      (if (< 300000 depth)
        (do
          (print "too deep")
          {:visited visited
           :direction direction
           :pos [row col]})

        (if (and (< -1 (nth next-pos 0) row-max)
                 (< -1 (nth next-pos 1) col-max))
          (recur next-pos
                 next-dir
                 (conj visited next-pos)
                 (inc depth))

          visited)))))

(defn solve-1 [input]
  ;(println input)
  (let [walls (find-walls input)
        start (start-point input)
        size (map-size input)]
    #_{:walls walls
       :start start
       :size size}

    (count (into #{} (do-walk walls start size)))))

(solve-1 (slurp "y2024/day06/example.txt"))
;(solve-1 (slurp "y2024/day06/input.txt"))

(defn do-walk-2 [walls start [row-max col-max]]
  (loop [[row col] start
         direction :north
         visited #{{:pos start :dir direction}}
         path [start]
         depth 1]

    (let [{:keys [next-pos next-dir]} (next-step walls direction row col)]
      ;(debug walls row-max col-max visited)

      (cond
        (< 6000 depth)
        :too-deep

        (contains? visited {:pos next-pos :dir next-dir})
        ;:loop
        [:loop {:pos next-pos :dir next-dir} path]

        (and (< -1 (nth next-pos 0) row-max)
             (< -1 (nth next-pos 1) col-max))
        (recur next-pos
               next-dir
               (conj visited {:pos next-pos :dir next-dir})
               (conj path next-pos)
               (inc depth))

        :else
        :exited))))

(defn solve-2 [input]
  ;(println input)
  (let [walls (find-walls input)
        start (start-point input)
        size (map-size input)
        _ (print start)

        patrol (set (do-walk walls start size))

        #_#_patrol (do-walk-2 walls start size)]
    ;(println patrol)
    (count (filter (partial not= :exited)
                   (for [obstacle (remove #{start} patrol)]
                     (when true #_(= [7 6] obstacle)
                           (do-walk-2 (conj walls obstacle) start size)))))))

;))

#_{:walls walls
   :start start
   :size size}

; 

(solve-2 (slurp "y2024/day06/example.txt"))
(solve-2 (slurp "y2024/day06/input.txt"))