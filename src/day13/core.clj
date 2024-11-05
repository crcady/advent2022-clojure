(ns day13.core
  (:require [clojure.string :as str]))

(defn check
  [left right & remaining-or-nil]
  (let [remaining (if (nil? remaining-or-nil) () (seq remaining-or-nil))]
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

(defn load-as-dicts
  [fname]
  (map (fn [lr n] {:id (inc n) :left (first lr) :right (second lr)}) (map str/split-lines (str/split (slurp (str "data/day13/" fname)) #"\n\n")) (range)))

(defn load-as-list
  [fname]
  (map #(read-string %) (filter not-empty (str/split-lines (slurp (str "data/day13/" fname))))))

(defn solve-first
  [fname]
  (reduce + (map :id (filter #(check (read-string (:left %)) (read-string (:right %))) (load-as-dicts fname)))))

(defn solve-second
  [fname]
  (let [mapped-lines (into {} (map (fn [item id] (vector item (inc id))) (sort check (conj (load-as-list fname) [[2]] [[6]])) (range)))]
    (* (get mapped-lines [[2]]) (get mapped-lines [[6]]))))
