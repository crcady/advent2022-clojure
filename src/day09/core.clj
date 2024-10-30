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
  [posns move]
  (case move
    "U" (assoc posns :hy (inc (:hy posns)))
    "D" (assoc posns :hy (dec (:hy posns)))
    "R" (assoc posns :hx (inc (:hx posns)))
    "L" (assoc posns :hx (dec (:hx posns)))))

(defn clamp
  [lower-bound upper-bound input]
  (min upper-bound (max lower-bound input)))

(def clamp1 (partial clamp -1 1))

(defn move-tail
  [posns]
  (let [hx (:hx posns) hy (:hy posns) tx (:tx posns) ty (:ty posns)
        dx (- hx tx) dy (- hy ty)]
    (if (or (= 0 dx) (= 0 dy)) ; Either saame row or same column
      (assoc posns :tx (+ tx (int (/ dx 2))) :ty (+ ty (int (/ dy 2))))
      (if (= (+ (abs dx) (abs dy)) 3) ; Diagonal and manhattan distance is 3
        (assoc posns :tx (+ tx (clamp1 dx)) :ty (+ ty (clamp1 dy)))
        posns))))

(def initial-position {:hx 0 :hy 0 :tx 0 :ty 0})

(defn moves-to-positions
  ([moves]
   (moves-to-positions (list initial-position) moves))
  ([positions moves]
   (if (empty? moves)
     positions
     (recur (cons (move-tail (move-head (first positions) (first moves))) positions) (rest moves)))))

(defn load-as-moves
  [fname]
  (process-line (str/split (slurp (str "data/day09/" fname)) #"\n")))

(defn solve-first
  [fname]
  (count (set (map (fn [x] [(:tx x) (:ty x)]) (moves-to-positions (load-as-moves fname))))))