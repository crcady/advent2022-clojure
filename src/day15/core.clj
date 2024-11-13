(ns day15.core
  (:require [clojure.string :as str]))

(defn load-as-coords
  [fname]
  (->> (slurp (str "data/day15/" fname))
       (str/split-lines)
       (map #(re-seq #"(-?\d+)" %))
       (map #(map second %))
       (map (fn [[sensor-x sensor-y beacon-x beacon-y]] {:sx (parse-long sensor-x) :sy (parse-long sensor-y) :bx (parse-long beacon-x) :by (parse-long beacon-y)}))))

(defn project-to-line
  [coord-list y]
  (let [middle-distances (map (fn [coords] {:middle (:sx coords) :distance (- (+ (abs (- (:sx coords) (:bx coords))) (abs (- (:sy coords) (:by coords)))) (abs (- (:sy coords) y)))}) coord-list)
        sorted-ranges (sort-by :left (map (fn [x] {:left (- (:middle x) (:distance x)) :right (+ (:middle x) (:distance x))}) (filter #(>= (:distance %) 0) middle-distances)))]
    (loop [finished () current (first sorted-ranges) remaining (rest sorted-ranges)]
      (if (empty? remaining)
        (cons current finished)
        (let [next (first remaining)]
          (cond
            (<= (:right next) (:right current)) (recur finished current (rest remaining)) ; Case: next range is contained within current range
            (> (:left next) (inc (:right current))) (recur (cons current finished) next (rest remaining)) ; Case: no overlap
            :else (recur finished (assoc current :right (:right next)) (rest remaining)))))))) ; Case: some overlap

(defn solve-first
  [fname y]
  (let [ranges (project-to-line (load-as-coords fname) y)]
    (reduce + (map #(- (:right %) (:left %)) ranges))))

(defn augment-coord
  [coordinate]
  (assoc coordinate :range (+ (abs (- (:sx coordinate) (:bx coordinate))) (abs (- (:sy coordinate) (:by coordinate))))))

(defn in-range
  [point sensor]
  (<= (+ (abs (- (:x point) (:sx sensor))) (abs (- (:y point) (:sy sensor)))) (:range sensor)))

(defn intersection
  [up-b down-b]
  (let [delta-b (- down-b up-b)]
    (if (even? delta-b)
      {:x (/ delta-b 2) :y (/ (+ up-b down-b) 2)}
      nil)))

(defn get-up-intercepts
  [coord]
  (list (+ (- (:sy coord) (:sx coord)) (inc (:range coord))) (- (- (:sy coord) (:sx coord)) (inc (:range coord)))))

(defn get-down-intercepts
  [coord]
  (list (+ (+ (:sy coord) (:sx coord)) (inc (:range coord))) (- (+ (:sy coord) (:sx coord)) (inc (:range coord)))))

(defn not-seen
  [sensors point]
  (not-any? true? (map #(in-range point %) sensors)))

(defn solve-second
  [fname square-size]
  (let [sensors (map augment-coord (load-as-coords fname))
        up-intercepts (reduce concat (map get-up-intercepts sensors))
        down-intercepts (reduce concat (map get-down-intercepts sensors))
        candidates (for [up-b up-intercepts
                         down-b down-intercepts
                         :let [int-point (intersection up-b down-b)]
                         :when (some? int-point)]
                     int-point)]
    (->> candidates
         (set)
         (filter #(and (>= (:x %) 0) (>= (:y %) 0) (<= (:x %) square-size) (<= (:y %) square-size)))
         (filter (partial not-seen sensors))
         (map #(+ (:y %) (* 4000000 (:x %))))
         (first))))
