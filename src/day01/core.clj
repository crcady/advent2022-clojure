(ns day01.core
  (:require [clojure.string :as str]))

(defn load-as-lines
  "Load a file out of the data/day01 directory as lines"
  [fname]
  (str/split (slurp (str "data/day01/" fname)) #"\n"))

(defn load-as-chunks
  "Load a file out of the data/day01 directory as chunks of lines"
  [fname]
  (str/split (slurp (str "data/day01/" fname)) #"\n\n"))

(defn chunk-value
  "Compute the calories an elf is carrying"
  [chunk]
  (apply + (map parse-long (str/split chunk #"\n"))))

(defn solve-first
  "Solves the first part of the puzzle for a given filename"
  [fname]
  (apply max (map chunk-value (load-as-chunks fname))))

(defn solve-second
  "Solves the second half of the puzzle"
  [fname]
  (apply + (take 3 (reverse (sort (map chunk-value (load-as-chunks fname)))))))