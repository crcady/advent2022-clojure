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

(defn solve-first
  [fname]
  (let [trees (load-as-woods fname)]
    (count (filter (partial visible trees) trees))))

