(ns day03.core
  (:require [clojure.string :as s]
            [clojure.pprint :refer [pprint]]))

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

(defn is-digit? [ch]
  (and ch (Character/isDigit ch)))

(is-digit? \4)
(is-digit? \a)

(defn is-symbol? [ch]
  (not (or (is-digit? ch)
           (= \. ch))))

(defn is-gear? [ch]
  (= \* ch))

(defn load-schematic [input]

  (let  [lines (s/split-lines input)
         lists (map-indexed
                (fn [y line]
                  (map-indexed
                   (fn [x char]
                     {[x y] char})
                   line))
                lines)]
    (apply merge (apply concat lists))))


(defn symbol-positions [schematic]
  (filter (comp is-symbol? val) schematic))

(defn gear-positions [schematic]
  (filter (comp is-gear? val) schematic))

(def neighbours
  [[-1 -1] [0 -1] [1 -1]
   [-1  0]         [1  0]
   [-1  1] [0  1] [1  1]])

(defn numbers-beside [schematic [x y]]
  (filter identity
          (for [[nx ny] neighbours]
            (let [coord [(+ nx x) (+ ny y)]
                  cell (get schematic coord)]
              (when (is-digit? cell)
                coord)))))


(def sc (load-schematic (slurp "day03/example.txt")))

(pprint sc)


(numbers-beside sc [3 1])

(defn load-number-at [schematic [x y]]
  (if (is-digit? (get schematic [(dec x) y]))
    (load-number-at schematic [(dec x) y])
    (let [digits (loop [i x
                        digits []]
                   (let [curr (get schematic [i y])]
                     (if (is-digit? curr)
                       (recur (inc i) (conj digits curr))
                       {:digits digits
                        :value (parse-long (apply str digits))
                        :coord [i y]})))]
      digits)))


(load-number-at sc [3 0])

(defn day3 [path]
  (let [schematic (load-schematic (slurp path))
        symbols (symbol-positions schematic)
        number-maps (for [[coord] symbols]
                      (let [nums (numbers-beside schematic coord)]
                        (map (partial load-number-at schematic) nums)))]
    (apply + (map :value (into #{} (apply concat number-maps))))))


(defn gear-ratio [number-maps]
  (when (= 2 (count number-maps))
    (apply * (map :value number-maps))))

(defn day3-part2 [path]
  (let [schematic (load-schematic (slurp path))
        symbols (gear-positions schematic)
        number-maps (for [[coord] symbols]
                      (let [nums (numbers-beside schematic coord)
                            digits-with-dupes (map (partial load-number-at schematic) nums)]
                        (into #{} digits-with-dupes)))]
    (->> number-maps
         (map gear-ratio)
         (filter identity)
         (apply +))))


(day3 "day03/example.txt")
(day3 "day03/input.txt")

(day3-part2 "day03/example.txt")
(day3-part2 "day03/input.txt")