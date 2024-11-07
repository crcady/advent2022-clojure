(ns day14.core
  (:require [clojure.string :as str]))

(def data-prefix "data/day14/")

;; Helpers

(defn clamp [lo hi x] (min (max x lo) hi))
(def unit-clamp (partial clamp -1 1))
(defn move-towards
  "Move point a one coordinate towards b. If they are not in a line, will move diagonal"
  [a b]
  (let [dx (- (:x b) (:x a)) dy (- (:y b) (:y a))]
    {:x (+ (:x a) (unit-clamp dx)) :y (+ (:y a) (unit-clamp dy))}))

;; These next four functions return the result of moving the sand.
;; They return nil if that move is not possible. That way they can be (and ...)ed together
;; Notably, y values go *up* as the sand falls, not down!

(defn down
  "Moves the sand only one point down, or else a free-falling sand would error."
  [rocks sand]
  (let [open-space  (not-any? #(and (= (:x %) (:x sand)) (= (:y %) (inc (:y sand)))) rocks)]
    (and open-space (assoc sand :y (inc (:y sand))))))

(defn down-left
  "Tries to move the grain of sand down and to the left."
  [rocks sand]
  (let [open-space  (not-any? #(and (= (:x %) (dec (:x sand))) (= (:y %) (inc (:y sand)))) rocks)]
    (and open-space (assoc sand :y (inc (:y sand)) :x (dec (:x sand))))))

(defn down-right
  "Tries to move the grain of sand down and to the right."
  [rocks sand]
  (let [open-space  (not-any? #(and (= (:x %) (inc (:x sand))) (= (:y %) (inc (:y sand)))) rocks)]
    (and open-space (assoc sand :y (inc (:y sand)) :x (inc (:x sand))))))

(defn apply-gravity
  "First tries down, then down-left, then down-right. Returns nil if the sand is stuck."
  [rocks sand]
  (or (down rocks sand) (down-left rocks sand) (down-right rocks sand)))

;; We need a function to check whether or not a sand particle is in freefall

(defn freefall
  [rocks sand]
  (let [rocks-below (filter #(and (= (:x %) (:x sand)) (> (:y %) (:y sand))) rocks)]
    (empty? rocks-below)))

;; Now we can combine those
(defn add-grain
  [start-point rocks]
  (loop [sand start-point]
    (let [next (apply-gravity rocks sand)]
      (if next
        (if (freefall rocks sand)
          rocks
          (recur next))
        (cons sand rocks)))))

(defn add-many-grains
  [start-point first-rocks]
  (loop [rocks first-rocks]
    (let [next (add-grain start-point rocks)]
      (if (= (count rocks) (count next))
        rocks
        (recur next)))))

(def fill (partial add-many-grains {:x 500 :y 0}))

(defn load-as-matches
  [fname]
  (map #(re-seq #"(\d+),(\d+)" %) (str/split-lines (slurp (str data-prefix fname)))))

(defn mk-paths
  [matches]
  (loop [current () remaining matches]
    (if-some [[_ x y] (first remaining)]
      (recur (cons {:x (parse-long x) :y (parse-long y)} current) (rest remaining))
      current)))

(defn paths-to-coords
  "Takes a list of start-end points and makes a list of coordinates that they fill"
  [paths]
  (loop [coord-list () a (first paths) b (second paths) remaining (drop 2 paths)]
    (if (= a b) ; We've moved the current a point all the way to b.
      (if (empty? remaining) ; We've handled all the paths
        (cons a coord-list) ; Make sure we grab the last point, and exit
        (recur coord-list b (first remaining) (rest remaining))) ; Keep going on next leg
      (recur (cons a coord-list) (move-towards a b) b remaining)))) ; Continue down the current leg

(defn point-list
  [fname]
  (into () (set (flatten (map paths-to-coords (map mk-paths (load-as-matches fname)))))))

(defn solve-first
  [fname]
  (let [rocks (point-list fname)]
    (- (count (fill rocks)) (count rocks))))