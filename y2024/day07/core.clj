(ns y2024.day07.core
  (:require [clojure.string :as s]
            [clojure.math.combinatorics :as c]))

(defn parse-1 [s]
  (let [[target & operands] (mapv parse-long (s/split (s/replace s ":" "") #" "))]
    {:target target
     :operands (vec operands)}))

(defn try-all-concat [strings]
  (let [candidates (c/selections [:concat :noop] (-> strings count dec))]
    (mapv (fn [ops]
            (loop [acc []
                   [a b & others] strings
                   operators ops]
              (let [f (first operators)]

                (case f
                  nil
                  (mapv parse-long (concat acc [a]))
                  :concat
                  (recur acc
                         (concat [(str a b)] others)
                         (rest operators))
                  :noop
                  (recur (conj acc a)
                         (concat [b] others)
                         (rest operators))))))
          candidates)))

(defn parse-2 [s]
  (let [[target & operands]  (s/split (s/replace s ":" "") #" ")]
    {:target (parse-long target)
     :operands (try-all-concat operands)}))


(try-all-concat ["a" "b" "c"])

(defn fn-name [f]
  (cond
    (= f +) "+"
    (= f *) "x"
    :else "||"))


(defn pipe-pipe [a b]

(parse-long (str a b))
)

(defn try-all [{:keys [target operands]}]
  (let [candidates (c/selections [+ *] (-> operands count dec))]
    (mapv (fn [ops]
            (loop [acc (first operands)
                   numbers (rest operands)
                   operators ops]

              (if-let [f (first operators)]
                (recur (f acc (first numbers))
                       (rest numbers)
                       (rest operators))
                #_(when (= acc target)
                  target)
                {:target target
                   :ops (mapv fn-name ops)
                   :numbers operands
                   :acc acc
                   :success (= acc target)})))
          candidates)))


(defn try-all-2 [{:keys [target operands]}]
  (let [candidates (c/selections [+ * pipe-pipe] (-> operands count dec))]
    (some (fn [ops]
            (loop [acc (first operands)
                   numbers (rest operands)
                   operators ops]

              (if-let [f (first operators)]
                (recur (f acc (first numbers))
                       (rest numbers)
                       (rest operators))
                (when (= acc target)
                  target)
                #_{:target target
                   :ops (mapv fn-name ops)
                   :numbers operands
                   :acc acc
                   :success (= acc target)})))
          candidates)))

(defn solve-1 [input]
  (reduce + (filter identity (mapv try-all (mapv parse-1 (s/split-lines input))))))


(solve-1 (slurp "y2024/day07/example.txt"))
(solve-1 (slurp "y2024/day07/input.txt"))

(defn solve-2 [input]
  (reduce + (filter identity (mapv try-all-2 (mapv parse-1 (s/split-lines input))))))

(solve-2 (slurp "y2024/day07/example.txt"))
(solve-2 (slurp "y2024/day07/input.txt"))
