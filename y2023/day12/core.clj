(ns day12.core
  (:require [clojure.string :as s]
            [clojure.set :as set]
            [clojure.test :refer [is]]
            [clojure.math.combinatorics :refer [cartesian-product]]
            [lib :refer [parse-ints transpose]]))


(defn ->regex [counts]
  ;\.*#{3}\.+#{2}\.+#{1}\.*
  (re-pattern (str "\\.*"
                   (apply str (interpose "\\.+" (map (partial format "#{%d}") counts)))
                   "\\.*")))

(->regex [3 2 1])

(defn permutations* [[ch & rst] result]
  (cond
    (nil? ch)
    (apply str result)

    (#{\. \#} ch)
    (permutations* rst (conj result ch))

    (= \? ch)
    [(permutations* rst (conj result \.))
     (permutations* rst (conj result \#))]

    :else
    (throw (Exception. "not handled"))))

(defn permutations [row]
  (flatten (permutations* (vec row) [])))


(count (permutations "?###????????"))

(defn count-arrangements [{:keys [row counts regex]}]
  (let [candidates (permutations row)]
    ;; (prn row)
    ;; (prn regex)
    ;; (prn candidates)
    ;; (prn "----")
    (count (filter (partial re-matches regex) candidates))))
    

(defn parse-arrangements [s]
  (let [[row counts] (s/split s #" ")
        counts (parse-ints (s/replace counts "," " "))]
    {:row row
     :counts counts
     :regex (->regex counts)}))

(defn part-1 [input]
  (->> input (s/split-lines)
       (mapv parse-arrangements)
       (mapv count-arrangements)
       (reduce +)))


(comment
  (part-1 "???.###????.###????.###????.###????.### 1,1,3,1,1,3,1,1,3,1,1,3,1,1,3")
  


  (part-1 (slurp "day12/example.txt"))
  
  (time 
   (part-1 (slurp "day12/input.txt")))
  )
