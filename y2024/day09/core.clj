(ns y2024.day09.core
  (:require [clojure.string :as s]
            [clojure.pprint :refer (pprint)]))


(defn expand [digits]
  (interpose "." (range (count digits))))

(defn parse-1 [input]
  (mapv (comp parse-long str) (seq input)))

(defn to-form-1 [input]

  (let [digits (parse-1 input)]
    (flatten (mapv (fn [digit value]
                     (repeat digit value)) digits (expand digits)))))


(defn to-form-2 [form-1]
  (let [count (count (filter number? form-1))]

    (loop [count count
           acc []
           input form-1
           filler (filter number? (reverse form-1))]

      (cond
        (zero? count)
        acc

        (= "." (first input))
        (recur (dec count)
               (conj acc (first filler))
               (rest input)
               (rest filler))

        (number? (first input))
        (recur (dec count)
               (conj acc (first input))
               (rest input)
               filler)

        :else
        {:acc acc :input input :filler filler}))))


(defn to-form-3 [form-2]
  (reduce + (map-indexed (fn [idx digit]
                           (* idx digit))
                         form-2)))


(defn solve-1 [input]
  (let [form-1 (to-form-1 input)
        form-1-str (apply str form-1)
        form-2 (to-form-2 form-1)
        form-2-str (apply str form-2)
        form-3 (to-form-3 form-2)]

    ;(prn "forms" input form-1-str form-2-str form-3)
    form-3))


(solve-1 "12345")
(solve-1 "2333133121414131402")
(solve-1 (s/trim (slurp "y2024/day09/input.txt")))


(defn to-form-a [input]

  (let [digits (parse-1 input)]
    (flatten (mapv (fn [digit value]
                     {:count digit
                      :file-no value})
                   digits
                   (expand digits)))))




(defn best-spot [form-a block]
  (let [current-index (.indexOf form-a block)](some
    (fn [[idx form]]
      (when (and (> current-index idx)
                 (= "." (:file-no form))
                 (<= (:count block) (:count form)))
        idx))
    (map-indexed vector form-a))))


(defn skwish [form-a]
  (loop [acc []
         [head & rest] form-a]
    (cond
      (nil? head)
      acc

      (number? (:file-no head))
      (recur (conj acc head) rest)

      (= "." (:file-no head))
      (let [[dots files] (split-with #(= "." (:file-no %)) rest)]

        (recur (conj acc {:file-no "."
                          :skwish (count dots)
                          :count (+ (:count head) (reduce + (map :count dots)))})

               files))

      :else
      {:error "unknown type" :acc acc :head head :rest rest})))

(defn move-to-slot [form-a form idx]
  #_(prn "moving" form "to" idx)
  (let  [without (mapv (fn [it]
                         (if (= it form)
                           (assoc it :file-no ".")
                           it)) form-a)
         head (take idx without)
         tail (drop (inc idx) without)
         site (nth without idx)
         extra (- (:count site) (:count form))]
    #_(prn "extra:" (:count site) (:count form)  extra)
    (if (pos? extra)
      (concat head [form {:count extra :file-no "." :extra extra}] tail)
      (concat head [form] tail))))


#_(skwish (move-to-slot [{:count 2, :file-no 0}
                         {:count 4, :file-no "."}
                         {:count 3, :file-no 1}
                         {:count 3, :file-no "."}] {:count 3, :file-no 1} 1))


(defn form-a->digits [form-a]
  (flatten (map (fn [{:keys [count file-no]}]
                  (repeat count file-no)) form-a)))

(defn print-form-a [form-a]
  (println (apply str (form-a->digits form-a))))

(defn to-form-b [form-a]
  #_(print-form-a form-a)
  (loop [[block & blocks] (reverse (filter (comp number? :file-no) form-a))
         current form-a]
    #_(print-form-a current)
    (if (nil? block)
      current
      (let [best (best-spot current block)]
        #_(when-not best
          (prn "no better spot for" (:file-no block)))
        (if (some? best)
          (recur blocks (skwish (move-to-slot current block best)))
          (recur blocks current))))))


(defn compute-checksum [digits]
  (reduce + (map-indexed (fn [idx digit]
                           (if (= "." digit)
                             0
                             (* idx digit)))
                         digits)))

(defn solve-2 [input]
  (let [form-a (to-form-a input)
        form-b (to-form-b form-a)
        checksum (compute-checksum (form-a->digits form-b))]
    checksum))


#_(solve-2 "12345")
(solve-2 "2333133121414131402")
(solve-2 (s/trim (slurp "y2024/day09/input.txt")))