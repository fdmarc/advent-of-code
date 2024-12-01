(ns day07.core
  (:require [clojure.string :as s]))

(def ->score-1 {"2" 2
                "3" 3
                "4" 4
                "5" 5
                "6" 6
                "7" 7
                "8" 8
                "9" 9
                "T" 10
                "J" 11
                "Q" 12
                "K" 13
                "A" 14})

(def ->score-2 {"2" 2
                "3" 3
                "4" 4
                "5" 5
                "6" 6
                "7" 7
                "8" 8
                "9" 9
                "T" 10
                "J" 1
                "Q" 12
                "K" 13
                "A" 14})


(defn parse-1 [s]
  (let [parts (re-find #"(\w)(\w)(\w)(\w)(\w)\s+(\d+)" s)]
    {:cards (vec (take 5 (rest parts)))
     :bid (parse-long (last parts))}))


(defn jacks-wild [cards]
  (let [indexed (group-by identity cards)]
    (if (contains? indexed "J")
      (replace {"J" (first (last (sort-by (comp count val) (dissoc indexed "J"))))}
               cards)
      cards)))

(defn with-best-hand [hand-fn hand]
  (assoc hand :best-hand (hand-fn (:cards hand))))

(jacks-wild ["3" "2" "T" "3" "J"])

(defn- with-values [score-fn {:keys [cards best-hand] :as hand}]
  (assoc hand :values (mapv score-fn cards)
         :best-values (mapv score-fn best-hand)))

; 6 Five of a kind, where all five cards have the same label: AAAAA
; 5 Four of a kind, where four cards have the same label and one card has a different label: AA8AA
; 4 Full house, where three cards have the same label, and the remaining two cards share a different label: 23332
; 3 Three of a kind, where three cards have the same label, and the remaining two cards are each different from any other card in the hand: TTT98
; 2 Two pair, where two cards share one label, two other cards share a second label, and the remaining card has a third label: 23432
; 1 One pair, where two cards share one label, and the other three cards have a different label from the pair and each other: A23A4
; 0 High card, where all cards' labels are distinct: 23456

(defn ->rank [counts]
  (cond
    (= [5] counts)
    {:rank 6 :type "five of a kind"}

    (= [4 1] counts)
    {:rank 5 :type "four of a kind"}

    (= [3 2] counts)
    {:rank 4 :type "full house"}

    (= [3 1 1] counts)
    {:rank 3 :type "three of a kind"}

    (= [2 2 1] counts)
    {:rank 2 :type "two pair"}

    (= [2 1 1 1] counts)
    {:rank 1 :type "one pair"}

    (= [1 1 1 1 1] counts)
    {:rank 0 :type "high card"}))

(defn- with-rank [{:keys [best-hand cards] :as hand}]
  (let [counts (vec (sort-by - (vals (update-vals (group-by identity best-hand) count))))
        jacks (->> cards (filter #{"J"}) count)]
    (merge  hand
            {:counts counts
             :jacks jacks}
            (->rank counts))))

(defn part-1-score [ranked]
  (reduce (fn [acc [index {:keys [bid]}]]
            (conj acc (* (inc index) bid)))
          []
          ranked))

(defn part-1 [score-fn hand-fn input]
  (let [lines (s/split-lines input)
        ranked (->> lines
                    (mapv parse-1)
                    (mapv (partial with-best-hand hand-fn))
                    (mapv (partial with-values score-fn))
                    (mapv with-rank)
                    (sort-by (juxt :rank :values))
                    (map-indexed vector))]
    [ranked (apply + (part-1-score ranked))]))


(part-1 ->score-1 identity (slurp "day07/example.txt")) ; 6440
(part-1 ->score-2 jacks-wild (slurp "day07/example.txt")) ; 6440
(part-1 ->score-2 jacks-wild (slurp "day07/example.txt")) ; 6440
(part-1 ->score-1 identity (slurp "day07/input.txt")) ; 250602641
(part-1 ->score-2 jacks-wild (slurp "day07/input.txt")) ; 251037509
