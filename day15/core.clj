(ns day15.core
  (:require [clojure.test :refer [is]]
            [clojure.string :as s]))

(defn elf-hash [s]
  (loop [curr 0
         [ch & rest] s]
    (if
     (nil? ch)
      curr
      (recur
       (mod  (* 17 (+ curr (int ch))) 256)
       rest))))

(+ 1 (int \a))
(elf-hash "rn=1")

(is (= 30 (elf-hash "rn=1")))
(is (= 253 (elf-hash "cm-")))
(is (= 97 (elf-hash "qp=3")))
(is (= 47 (elf-hash "cm=2")))
(is (= 14 (elf-hash "qp-")))
(is (= 180 (elf-hash "pc=4")))
(is (= 9 (elf-hash "ot=9")))
(is (= 197 (elf-hash "ab=5")))
(is (= 48 (elf-hash "pc-")))
(is (= 214 (elf-hash "pc=6")))
(is (= 231 (elf-hash "ot=7")))

(defn part-1 [input]
  (reduce + (mapv elf-hash (s/split input #","))))

(is (= 1320 (part-1 (slurp "day15/example.txt"))))
(is (= 511343 (part-1 (slurp "day15/input.txt"))))