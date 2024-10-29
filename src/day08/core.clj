(ns day08.core)

(def tiny-woods
  "122
   113
   112"
  '({:row 0 :col 0 :height 1}
    {:row 0 :col 1 :height 2}
    {:row 0 :col 2 :height 2}
    {:row 1 :col 0 :height 1}
    {:row 1 :col 1 :height 1}
    {:row 1 :col 2 :height 3}
    {:row 2 :col 0 :height 1}
    {:row 2 :col 1 :height 1}
    {:row 2 :col 2 :height 2}))

(defn string-to-woods
  [accum remaining]
  (if (empty? remaining)
    accum
    (let [current-row (:row accum)
          current-col (:col accum)
          current-list (:trees accum)]
      (if (= (first remaining) \newline)
        (recur {:row (inc current-row) :col 0 :trees current-list} (rest remaining))
        (recur {:row current-row :col (inc current-col) :trees (cons {:row current-row :col current-col :height (parse-long (str (first remaining)))} current-list)} (rest remaining))))))

(defn load-as-woods
  [fname]
  (:trees (string-to-woods {:row 0 :col 0 :trees ()} (slurp (str "data/day08/" fname)))))

(defn blocked-up
  [forest tree]
  (some #(and
          (= (:col %) (:col tree))
          (< (:row %) (:row tree))
          (>= (:height %) (:height tree)))
        forest))

(defn blocked-down
  [forest tree]
  (some #(and
          (= (:col %) (:col tree))
          (> (:row %) (:row tree))
          (>= (:height %) (:height tree)))
        forest))

(defn blocked-left
  [forest tree]
  (some #(and
          (< (:col %) (:col tree))
          (= (:row %) (:row tree))
          (>= (:height %) (:height tree)))
        forest))

(defn blocked-right
  [forest tree]
  (some #(and
          (> (:col %) (:col tree))
          (= (:row %) (:row tree))
          (>= (:height %) (:height tree)))
        forest))

(defn hidden
  "Check whether a tree is hidden or not"
  [forest tree]
  (and (blocked-up forest tree) (blocked-down forest tree) (blocked-left forest tree) (blocked-right forest tree)))

(def visible (complement hidden))

(defn scenic-up
  [forest tree]
  (let [taller-trees (filter #(and (= (:col %) (:col tree)) (< (:row %) (:row tree)) (>= (:height %) (:height tree))) forest)]
    (if (empty? taller-trees)
      (:row tree)
      (- (:row tree) (apply max (map :row taller-trees))))))

(defn scenic-down
  [forest tree]
  (let [taller-trees (filter #(and (= (:col %) (:col tree)) (> (:row %) (:row tree)) (>= (:height %) (:height tree))) forest)]
    (if (empty? taller-trees)
      (- (apply max (map :row forest)) (:row tree))
      (- (apply min (map :row taller-trees)) (:row tree)))))

(defn scenic-left
  [forest tree]
  (let [taller-trees (filter #(and (< (:col %) (:col tree)) (= (:row %) (:row tree)) (>= (:height %) (:height tree))) forest)]
    (if (empty? taller-trees)
      (:col tree)
      (- (:col tree) (apply max (map :col taller-trees))))))

(defn scenic-right
  [forest tree]
  (let [taller-trees (filter #(and (> (:col %) (:col tree)) (= (:row %) (:row tree)) (>= (:height %) (:height tree))) forest)]
    (if (empty? taller-trees)
      (- (apply max (map :col forest)) (:col tree))
      (- (apply min (map :col taller-trees)) (:col tree)))))

(defn scenic-score
  [forest tree]
  (* (scenic-up forest tree) (scenic-down forest tree) (scenic-left forest tree) (scenic-right forest tree)))

(defn solve-first
  [fname]
  (let [trees (load-as-woods fname)]
    (count (filter (partial visible trees) trees))))

(defn solve-second
  [fname]
  (let [trees (load-as-woods fname)]
    (apply max (map (partial scenic-score trees) trees))))