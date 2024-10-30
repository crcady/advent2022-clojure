(ns day09.core
  (:require [clojure.string :as str]))

(defn process-line
  ([remaining]
   (process-line () remaining))
  ([so-far remaining]
   (if (empty? remaining)
     so-far
     (let [[direction how-many] (str/split (first remaining) #" ")]
       (recur (concat so-far (repeat (parse-long how-many) direction)) (rest remaining))))))

(defn move-head
  [point move]
  (case move
    "U" (assoc point :y (inc (:y point)))
    "D" (assoc point :y (dec (:y point)))
    "R" (assoc point :x (inc (:x point)))
    "L" (assoc point :x (dec (:x point)))))

(defn clamp
  [lower-bound upper-bound input]
  (min upper-bound (max lower-bound input)))

(def clamp1 (partial clamp -1 1))

(defn move-tail
  [head tail]
  (let [hx (:x head) hy (:y head) tx (:x tail) ty (:y tail)
        dx (- hx tx) dy (- hy ty)]
    (if (or (= 0 dx) (= 0 dy)) ; Either saame row or same column
      (assoc tail :x (+ tx (int (/ dx 2))) :y (+ ty (int (/ dy 2))))
      (if (>= (+ (abs dx) (abs dy)) 3) ; Diagonal and manhattan distance is 3 or more
        (assoc tail :x (+ tx (clamp1 dx)) :y (+ ty (clamp1 dy)))
        tail))))

(defn follow-tail
  [moved remaining]
  (if (empty? remaining)
    moved
    (recur (cons (move-tail (first moved) (first remaining)) moved) (rest remaining))))

(defn apply-move
  [posns move]
  (let [remaining (reverse posns)]
    (follow-tail (list (move-head (first remaining) move)) (rest remaining))))

(defn initial-position
  [n]
  (repeat n {:x 0 :y 0}))

(defn moves-to-positions
  ([positions moves] ; positions is a list of lists of maps
   (if (empty? moves)
     positions
     (recur (cons (apply-move (first positions) (first moves)) positions) (rest moves)))))

(defn load-as-moves
  [fname]
  (process-line (str/split (slurp (str "data/day09/" fname)) #"\n")))

(defn solve-first
  [fname]
  (count (set (map (fn [x] (first x)) (moves-to-positions (list (initial-position 2)) (load-as-moves fname))))))

(defn solve-second
  [fname]
  (count (set (map (fn [x] (first x)) (moves-to-positions (list (initial-position 10)) (load-as-moves fname))))))