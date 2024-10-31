(ns day10.core
  (:require [clojure.string :as str]))

(defn load-as-instructions
  [fname]
  (map
   #(let [v (str/split % #" ")] (if (= (count v) 1) [:noop] [:addx (parse-long (second v))]))
   (str/split (slurp (str "data/day10/" fname)) #"\n")))

(defn i-to-c
  [ins]
  (case (first ins)
    :noop [1 0]
    :addx [2 (second ins)]))

(defn instrs-to-commits
  ([instrs]
   (instrs-to-commits () instrs 0))
  ([commits instrs current]
   (if (empty? instrs)
     commits
     (let [[t x] (i-to-c (first instrs))]
       (recur (cons {:time (+ current t) :value x} commits) (rest instrs) (+ current t))))))

(defn get-x-at
  [commits n]
  (reduce + 1 (map :value (take-while #(< (:time %) n) commits))))

(defn signal-strength
  [t commits]
  (* t (get-x-at commits t)))

(defn get-pixel
  [commits i]
  (if (< (abs (- (get-x-at commits i) (mod (dec i) 40))) 2)
    "#"
    "."))

(defn load-as-commits
  [fname]
  (reverse (instrs-to-commits (load-as-instructions fname))))

(defn solve-first
  [fname]
  (let [c (load-as-commits fname)]
    (+ (signal-strength 20 c)
       (signal-strength 60 c)
       (signal-strength 100 c)
       (signal-strength 140 c)
       (signal-strength 180 c)
       (signal-strength 220 c))))

(defn solve-second
  [fname]
  (let [c (load-as-commits fname)]
    (println (apply str (map #(get-pixel c %) (range 1 41))))
    (println (apply str (map #(get-pixel c %) (range 41 81))))
    (println (apply str (map #(get-pixel c %) (range 81 121))))
    (println (apply str (map #(get-pixel c %) (range 121 161))))
    (println (apply str (map #(get-pixel c %) (range 161 201))))
    (println (apply str (map #(get-pixel c %) (range 201 241))))))