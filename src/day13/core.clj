(ns day13.core)

(defn check
  [left right & remaining-or-nil]
  (let [remaining (if (nil? remaining-or-nil) () (seq remaining-or-nil))]
    (println left right remaining-or-nil "->" remaining)
    (cond
      (and (number? left) (number? right)) (if (= left right)
                                             (recur (first remaining) (second remaining) (vec (drop 2 remaining)))
                                             (< left right))
      (and (number? left) (vector? right)) (recur [left] right remaining)
      (and (vector? left) (number? right)) (recur left [right] remaining)
      (and (vector? left) (vector? right)) (let [first-left (first left) first-right (first right)]
                                             (cond
                                               (and first-left first-right) (recur first-left first-right (conj remaining (vec (rest right)) (vec (rest left))))
                                               (and (nil? first-left) (nil? first-right)) (recur (first remaining) (second remaining) (vec (drop 2 remaining)))
                                               :else (nil? first-left)))
      :else (throw (Exception. "Unexpected types"))))) ; This will throw if two identical vectors are thrown at it - which is illegal per the problem setup