(ns day17.core
  (:require
   [clojure.string :as str]))

(defn mk-piece
  [offset, index]
  (case index
    0 (map (fn [x] {:x x :y offset}) (range 2 6))
    1 (concat `(~{:x 3 :y offset} ~{:x 3 :y (+ 2 offset)}) (map (fn [x] {:x x :y (inc offset)}) (range 2 5)))
    2 (concat `(~{:x 4 :y (inc offset)} ~{:x 4 :y (+ 2 offset)}) (map (fn [x] {:x x :y offset}) (range 2 5)))
    3 (map (fn [y] {:x 2 :y (+ y offset)}) (range 4))
    4 (for [x [2 3] y [offset (inc offset)]] {:x x :y y})))

(defn down
  [pieces]
  (map (fn [{x :x y :y}] {:x x :y (dec y)}) pieces))

(defn left
  [pieces]
  (map (fn [{x :x y :y}] {:x (dec x) :y y}) pieces))

(defn right
  [pieces]
  (map (fn [{x :x y :y}] {:x (inc x) :y y}) pieces))

(defn is-open
  [rows {x :x y :y}]
  (cond
    (<= y 0) false ; Hit floor 
    (< x 0) false ; Hit left wall
    (> x 6) false ; Hit right wall 
    (> y (:num @(first rows))) true ; Above any piece 
    :else (not (nth (:values @(first (filter (fn [x] (= (:num @x) y)) rows))) x)))) ; Check the actual board

(defn check-pieces
  "True when every piece is legally placed"
  [rows pieces]
  (reduce #(and %1 %2) (map #(is-open rows %) pieces)))

(defn extend-rows
  [rows height]
  (loop [rows rows]
    (if (< (:num @(first rows)) height)
      (recur (cons (atom {:num (inc (:num @(first rows))) :values [false false false false false false false]}) rows))
      rows)))

(defn write-piece
  "Write a single piece into an (already extended) row table"
  [rows {x :x y :y}]
  (let [old-row (first (filter (fn [x] (= (:num @x) y)) rows)) old-values (:values @old-row)]
    (swap! old-row assoc :values (assoc old-values x true))
    rows))

(defn sweep
  [rows]
  (let [swept-list (take-while (fn [x] (not= (:values @x) [true true true true true true true])) rows)]
    (if (< (count swept-list) (count rows))
      (take (inc (count swept-list)) rows)
      rows)))

(defn write-to-rows
  "Write pieces into a row table"
  [rows pieces]
  (let [max-height (apply max (map :y pieces)) new-rows (extend-rows rows max-height)]
    (sweep (reduce #(write-piece %1 %2) new-rows pieces))))

(defn apply-gravity
  "Tries to move the current pieces down, if it can, does so. If not, it saves it into the state and spawns a new piece."
  [{current-pieces :current-pieces rows :rows piece-count :piece-count :as current-state}]
  (let [down-pieces (down current-pieces)]
    (if (check-pieces rows down-pieces)
      (assoc current-state :current-pieces down-pieces)
      (let [new-rows (write-to-rows rows current-pieces) new-piece-count (inc piece-count)]
        (assoc current-state
               :rows new-rows
               :piece-count new-piece-count
               :current-pieces (mk-piece (+ (:num @(first new-rows)) 4) (mod new-piece-count 5)))))))

(defn apply-wind
  "Tries to move the pieces one position left or right. Updates wind index."
  [{current-pieces :current-pieces rows :rows wind :wind wind-index :wind-index :as current-state}]
  (let [this-wind (nth wind wind-index)
        new-index (mod (inc wind-index) (count wind))
        try-pieces (if (= this-wind \<) (left current-pieces) (right current-pieces))
        new-pieces (if (check-pieces rows try-pieces) try-pieces current-pieces)]
    (assoc current-state :wind-index new-index :current-pieces new-pieces)))

(defn load-as-wind
  [fname]
  (str/trim (slurp (str "data/day17/" fname))))

(defn initial-state
  [wind]
  {:rows (list (atom {:num 0 :values [true true true true true true true]}))
   :wind wind
   :wind-index 0
   :current-pieces (mk-piece 4 0)
   :piece-count 0})

(defn solve-first
  [fname]
  (loop [state (initial-state (load-as-wind fname))]
    (if (= 2022 (:piece-count state))
      (:num @(first (:rows state)))
      (recur (apply-gravity (apply-wind state))))))

(defn solve-nth
  [fname num]
  (loop [state (initial-state (load-as-wind fname))]
    (if (= num (:piece-count state))
      (:num @(first (:rows state)))
      (recur (apply-gravity (apply-wind state))))))

(defn make-key
  [state]
  (let [values (map (fn [x] (:values @x)) (:rows state))]
    values))

(defn make-value
  [state]
  (:piece-count state))

(defn run-until-new-piece
  [{piece-count :piece-count :as state}]
  (loop [next-state (apply-gravity (apply-wind state))]
    (if (= piece-count (:piece-count next-state))
      (recur (apply-gravity (apply-wind next-state)))
      next-state)))

(defn solve-second
  [fname]
  (loop [state (initial-state (load-as-wind fname)) cache {}]
    (let [next-state (run-until-new-piece state)
          cache-hit (get cache (make-key next-state))]
      (if cache-hit
        (println "Got a hit after" (:piece-count next-state))
        (recur next-state (assoc cache (make-key next-state) (make-value next-state)))))))