(ns day03.core
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn load-as-lines
  "Load a file out of the data/day03 directory as lines"
  [fname]
  (str/split (slurp (str "data/day03/" fname)) #"\n"))

(defn priority
  "Lowercase item types a-z have priority 1-26. Uppercase A-Z have priority 27-52"
  [item]
  (let [i (int item)]
    (if (and (> i 96) (< i 123)) ; True when item in [a-z]
      (- i 96)
      (- i 38))))

(defn get-item
  "Get the item that is in both compartments"
  [line]
  (let [n (/ (count line) 2), a (into #{} (take n line)), b (into #{} (drop n line))]
    (first (set/intersection a b))))

(defn solve-first
  "Solve the first half of the puzzle"
  [fname]
  (->> (load-as-lines fname)
       (map get-item)
       (map priority)
       (apply +)))

(defn badge-priority
  "Recursively gets the priority of badges"
  ([a b c] (let [set-a (into #{} a), set-b (into #{} b), set-c (into #{} c)] (priority (first (set/intersection set-a set-b set-c)))))
  ([a b c & others] (+ (badge-priority a b c) (apply badge-priority others))))

(defn solve-second
  "Solve the second half of the puzzle"
  [fname]
  (apply badge-priority (load-as-lines fname)))