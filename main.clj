(require '[clojure.string :as s])

(def stems
  [["0" 0]
   ["1" 1]
   ["2" 2]
   ["3" 3]
   ["4" 4]
   ["5" 5]
   ["6" 6]
   ["7" 7]
   ["8" 8]
   ["9" 9]
   ["zero" 0]
   ["one" 1]
   ["two" 2]
   ["three" 3]
   ["four" 4]
   ["five" 5]
   ["six" 6]
   ["seven" 7]
   ["eight" 8]
   ["nine" 9]])

(defn s->digit [s]
  (loop [[[sv dv] & rst] stems]
    (if (nil? sv)
      nil
      (if (s/starts-with? s sv)
        dv
        (recur rst)))))

(defn strings [s]
  (loop [s s
         acc []]
    (if (s/blank? s)
      acc
      (recur (.substring s 1)
             (conj acc s)))))

(defn s->digits [s]
  (->> s
       (strings)
       (map s->digit)
       (filter identity)))

(s->digits "one2")

(s->digits "abcone2threexyz")

(defn day1 [path]
  (let  [lines (s/split-lines (slurp path))]
    (->> lines
         (map s->digits)
         (map (juxt first last))
         (map (fn [[a b]] (+ (* 10 a) b)))
         (apply +))))

(day1 "day01/example.txt")
(day1 "day01/example2.txt")
(day1 "day01/input.txt")


(defn parse-count [s]
  (let  [[_ n colour] (re-matches  #"(\d+) (\w+)" s)]
    [colour (parse-long n)]))

(defn parse-result [s]
  (let [matches (s/split s #", ")]
    (into {} (map parse-count matches))))

(defn parse-game [line]
  (let [[game result] (s/split line #": ")
        results (s/split  result #"; ")]

    [(parse-long (re-find #"\d+" game))
     (map parse-result results)]))


(defn possible-result? [{:strs [red green blue]
                         :or {red 0 green 0 blue 0}}]
  (and (<= red 12)
       (<= green 13)
       (<= blue 14)))

(possible-result? {"red" 13})

(defn possible? [[_ results]]
  (every? possible-result? results))


(possible? ["1" [{"red" 12} {"blue" 111}]])


(defn fewest-cubes [results] 
 (apply merge-with max results))

(fewest-cubes (list {"blue" 3, "red" 4} {"red" 1, "green" 2, "blue" 6} {"green" 2}))

(defn power [results]
  (apply * (vals (fewest-cubes results))))
  

(power (list {"blue" 3, "red" 4} {"red" 1, "green" 2, "blue" 6} {"green" 2}))

(defn day2 [path]
  (let  [lines (s/split-lines (slurp path))
         games (map parse-game lines)]
    (->> games
         (filter possible?)
         (map first)
         (apply +))))


(defn day2-part2 [path]
  (let  [lines (s/split-lines (slurp path))
         games (map parse-game lines)]
    (apply + (for [[_ results] games]
       (power results)))))
    

(day2 "day02/example.txt")
(day2 "day02/input.txt")
(day2-part2 "day02/example.txt")
(day2-part2 "day02/input.txt")


