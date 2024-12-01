(ns day08.core

  (:require [clojure.string :as s]
            [clojure.test :refer [is]]
            [lib :refer [lcmv]]
            ))

(defn parse-1 [s]
  (->> s
       (re-seq #"(\w+) = \((\w+), (\w+)\)")
       (mapv rest)
       (mapv (juxt first rest))
       (into {})))

(defn traverse [nodes instructions]
  (let [bytecode (cycle (replace {\L first \R second} instructions))]
    (loop [location "AAA"
           [inst & others] bytecode
           steps 0]
      (if (= location "ZZZ")
        steps
        (recur
         (inst (get nodes location))
         others
         (inc steps))))))

(defn part-1 [input]
  (let [lines (s/split-lines input)
        instructions (seq (first lines))
        nodes (parse-1 input)]
    (traverse nodes instructions)))

(defn find-start-nodes [nodes]
  (->> nodes
       (keys)
       (filter #(s/ends-with? % "A"))))


(defn traverse-2 [nodes instructions location]
  (let [bytecode (cycle (replace {\L first \R second} instructions))
        terminal-nodes (set (filter #(s/ends-with? % "Z") (keys nodes)))]
    (prn "looking from" location " to " terminal-nodes)
    (loop [current location
           [inst & others] bytecode
           steps 0
           result []]
      (cond
        ;(every? terminal-nodes locations)
        ;steps

        (> steps 100000)
        result

        :else
        (recur (inst (get nodes current))
               others
               (inc steps)
               (if (terminal-nodes current)
                 (conj result steps)
                 result))))))

(defn part-2 [input]
  (let [lines (s/split-lines input)
        instructions (seq (first lines))
        nodes (parse-1 input)
        start (find-start-nodes nodes)
        paths (mapv (partial traverse-2 nodes instructions) start)
        hits (mapv first paths )
        ]
    (apply lcmv hits)))



(comment
  (is (= 2 (part-1 (slurp "day08/example1.txt"))))

  (is (= 6 (part-1 (slurp "day08/example2.txt"))))


  (part-1  (slurp "day08/input.txt"))



  (is (= 6 (part-2 (slurp "day08/example3.txt")))))
(part-2 (slurp "day08/input.txt"))
