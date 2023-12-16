(ns lib
  (:require [clojure.string :as s]))

(defn parse-ints [s]
  (->>
   (s/split s #"\s+")
   (filter (complement s/blank?))
   (mapv s/trim)
   (mapv parse-long)))

(parse-ints "  12 2   2   2")