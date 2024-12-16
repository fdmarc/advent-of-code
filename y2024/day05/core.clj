(ns y2024.day05.core
  (:require [clojure.string :as s]
            [clojure.pprint :refer [pprint]]
            [clojure.math :refer (floor)]))

(defn find-rules [input]
  (map (fn [[_ x y]]
         {:x (parse-long x)
          :y (parse-long y)})
       (re-seq #"(\d+)\|(\d+)" input)))


(defn find-pages [input]
  (let [lines (s/split-lines input)
        page-part (drop-while #(or (s/blank? %)
                                   (s/includes? % "|")) lines)]
    (mapv (partial mapv parse-long) (mapv #(s/split % #",") page-part))))



(defn to-lookup [page-list]
  (into {} (map-indexed (fn [index page]
                          [page index])
                        page-list)))


(defn middle-value [vect]
  (when-not (empty? vect)
    (vect (quot (count vect) 2))))

(middle-value [1 2 3])

(defn check-pages [rules pages]
  (let [lookup (to-lookup pages)

        results (for [{:keys [x y]} rules]
                  (let [x-pos (get lookup x)
                        y-pos (get lookup y)]
                    (when (and x-pos y-pos)
                      (if (< x-pos y-pos)
                        #_(format "PASS %d @ %d is less than %d @ %d" x x-pos y y-pos)
                        nil
                        (format "FAIL %d @ %d is less than %d @ %d" x x-pos y y-pos)))))

        failures (filter identity results)]

    (when (empty? failures)
      (middle-value pages))))

(defn solve-1 [input]
  (let [rules (find-rules input)
        pages (find-pages input)]


    (reduce + (filter identity (mapv (partial check-pages rules) pages)))))


(to-lookup [20 21 22])


(solve-1 (slurp "y2024/day05/example.txt"))
(solve-1 (slurp "y2024/day05/input.txt"))



(defn order-rules-impl [rules acc depth]
  ;(print rules)
  (cond
    (= 1  (count rules))
    (into [] (concat acc [(-> rules first :x) (-> rules first :y)]))

    (< 10 depth)
    acc

    :else

    (let [all-pages (into #{} (mapcat (juxt :x :y) rules))
          seed (reduce (fn [acc p]
                         (assoc acc p 0))
                       {}
                       all-pages)
          ;_ (pprint "seed")
          ;_ (pprint seed)
          afters (reduce (fn [acc {:keys [x y]}]
                           (update acc y inc))

                         seed
                         rules)


          ordered (sort-by :freq (mapv (fn [[page freq]]
                           {:page page
                            :freq freq})
                         afters))

          _ (print ordered)

          lowest (:page (first ordered))]

      (print "lowest is ")
      (print lowest)
      (recur
       (remove (fn [{:keys [x y]}]
                 (= x lowest)) rules)

       (conj acc lowest)
       (inc depth)))))

(defn order-rules [rules]
  (order-rules-impl rules [] 0))

(defn solve-2 [input]
  (let [rules (find-rules input)
        pages (find-pages input)
        result (order-rules rules)]
    result))



;(solve-2 (slurp "y2024/day05/example.txt"))
(solve-2 (slurp "y2024/day05/input.txt"))