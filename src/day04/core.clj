(ns day04.core
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn load-as-lines
  "Load a file out of the data/day04 directory as lines"
  [fname]
  (str/split (slurp (str "data/day04/" fname)) #"\n"))

(defn line-to-numbers
  "Takes a string in the form 2-4,6-8 and returns (2 4 6 8)"
  [line]
  (->> (re-matches #"(\d+)-(\d+),(\d+)-(\d+)" line)
       (drop 1)
       (map parse-long)))

(defn fully-overlapping
  "Given 2 ranges as 4 longs, check if the ranges completely overlap"
  [x1 x2 y1 y2]
  (or
   (or (= x1 y1) (= x2 y2)) ; If the beginning or end points are the same, they must overlap
   (if (<= x1 y1)
     (>= x2 y2)
     (<= x2 y2))))

(defn solve-first
  "Solve the first puzzle of day 4"
  [fname]
  (->> (load-as-lines fname)
       (map line-to-numbers)
       (filter #(apply fully-overlapping %))
       (count)))

(defn overlapping
  "Givern 2 ranges as 4 longs, check if they overlap at all"
  [x1 x2 y1 y2]
  (let [least-right (min x2 y2), most-left (max x1 y1)]
    (<= most-left least-right)))

(defn solve-second
  "Solve the second puzzle of day 4"
  [fname]
  (->> (load-as-lines fname)
       (map line-to-numbers)
       (filter #(apply overlapping %))
       (count)))