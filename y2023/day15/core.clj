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


(defn ->instruction [s]
  (if-let [m (re-find #"(\w+)=(\d+)" s)]
    {:op \=
     :input s
     :focal-length (parse-long (last m))
     :label (second m)}
    {:op \-
     :input s
     :label (subs s 0 (dec (count s)))}))

(defn with-box [{:keys [label] :as step}]
  (assoc step :box (elf-hash label)))

(defn parse [input]
  (->> #","
       (s/split input)
       (mapv ->instruction)
       (mapv with-box)))


(defmulti operate* (fn [_box {:keys [op]}] op))

(defmethod operate* \- [box {:keys [label]}]
  (vec (remove (fn [lens] (= (:label lens) label))
               box)))

(defmethod operate* \= [box {:keys [label] :as step}]
  (let [labels (set (mapv :label box))]
    (if (contains? labels label)
      (mapv
       (fn [lens]
         (if (= (:label lens) (:label step))
           step
           lens))

       box)
      (conj box step))))

(defn display-lens [lens]
  (format "[%s %s]" (:label lens) (:focal-length lens)))

(defn display [step boxes]
  (apply str
         (format "After \"%s\"\n" (:input step))

         (map-indexed (fn [idx box]
                        (when (seq box)
                          (format "Box %d: %s\n" idx (s/join " " (map display-lens box)))))
                      boxes)))


(defn operate [boxes {:keys [box] :as step}]
  (let [result (update boxes box operate*  step)]
    #_(println (display step result))
    result))

(defn score* [box-index box]
  (map-indexed (fn [slot-index {:keys [label focal-length]}]
                 (let [power (* (inc box-index)
                                (inc slot-index)
                                focal-length)]
                   (printf "%s: %d (box %d) * %d (slot) * %d (focal length) = %d\n"
                            label (inc box-index) box-index (inc slot-index) focal-length
                            power)

                   power))
               box))

(defn score [boxes]
  (reduce + (apply concat (map-indexed score* boxes))))


(defn part-2 [input]
  (let [steps (parse input)]
    (println (score (reduce operate (vec (repeat 256 [])) steps)))))




(part-2 (slurp "day15/example.txt"))
(part-2 (slurp "day15/input.txt"))
