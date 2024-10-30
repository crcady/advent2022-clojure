(ns day09.core
  (:require [clojure.string :as str]))

(defn process-line
  ([remaining]
   (process-line () remaining))
  ([so-far remaining]
   (if (empty? remaining)
     so-far
     (let [[direction how-many] (str/split (first remaining) #" ")]
       (recur (concat (repeat (parse-long how-many) direction) so-far) (rest remaining))))))

(defn load-as-moves
  [fname]
  (process-line (str/split (slurp (str "data/day09/" fname)) #"\n")))