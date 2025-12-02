(ns y2024.day13.core)


(def regex #"Button A: X([\d\+\-]+), Y([\d\+\-]+)\nButton B: X([\d\+\-]+), Y([\d\+\-]+)\nPrize: X=([\d\+\-]+), Y=([\d\+\-]+)")

(defn parse-1 [path]
  (for [game (re-seq regex (slurp path))]
    (let [[ax ay bx by px py] (map parse-long (rest game))]
      {:a [ax ay]
       :b [bx by]
       :prize [px py]})))

(parse-1 "y2024/day13/example.txt")
(parse-1 "y2024/day13/input.txt")


(defn presses [dx dy]
  (take 100 (iterate (fn [{:keys [x y count]}]
                       {:x (+ dx x)
                        :y (+ dy y)
                        :count (inc count)})
                     {:x 0 :y 0 :count 0})))

(defn cost [price {:keys [count]}]
  (* price count))

(defn estimate-game [{:keys [a b prize]}]
  (let [[px py] prize
        [ax ay] a
        [bx by] b
        px (+ px 10000000000000)
        py (+ py 10000000000000)
        _ (prn px py ax ay bx by)
        min-a (min
               (long (Math/floor (/ px ax)))
               (long (Math/floor (/ py ay))))
        min-b (min
               (long (Math/floor (/ px bx)))
               (long (Math/floor (/ py by))))]

    {:min-a min-a
     :min-b min-b}))


(defn solve-game-1 [{:keys [a b prize]}]
  (let [[px py] prize
        a-presses (apply presses a)
        b-presses (apply presses b)]
    (last
     (sort-by :cost
              (filter identity
                      (for [a-press a-presses
                            b-press b-presses]
                        (when (and (= px (+ (:x a-press) (:x b-press)))
                                   (= py (+ (:y a-press) (:y b-press))))
                          (let [cost-a (cost 3 a-press)
                                cost-b (cost 1 b-press)]
                            {:a a-press
                             :b b-press
                             :cost-a cost-a
                             :cost-b cost-b
                             :cost (+ cost-a cost-b)}))))))))

(defn solve-1 [path]
  (let [results (mapv :cost (mapv solve-game-1 (parse-1 path)))]
    (prn results)
    (reduce + (filter identity results))))

(comment
  (solve-1 "y2024/day13/example.txt"))
(comment
  (solve-1 "y2024/day13/input.txt"))

(defn solve-2 [path]
  (let [results (mapv estimate-game (parse-1 path))]
    (prn results)))
     

(/ 20 3)

(solve-2 "y2024/day13/example.txt")
