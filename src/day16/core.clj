(ns day16.core
  (:require [clojure.string :as str])
  (:require [clojure.set :as c-set]))

(def correct-positions {"AA" #{"BB" "CC" "DD" "EE" "HH" "JJ"}
                        "DD" #{"BB" "CC" "EE" "HH" "JJ"}
                        "BB" #{"CC" "EE" "HH" "JJ"}
                        "JJ" #{"CC" "EE" "HH"}
                        "HH" #{"CC" "EE"}
                        "EE" #{"CC"}
                        "CC" #{}})

(defn line-to-map
  [line]
  (let [tunnel-ids (re-seq #"[A-Z][A-Z]" line)
        flow-rate (re-find #"\d+" line)]
    {:id (first tunnel-ids)
     :flow-rate (parse-long flow-rate)
     :destinations (rest tunnel-ids)}))

(defn load-to-list
  [fname]
  (map line-to-map (str/split-lines (slurp (str "data/day16/" fname)))))

(defn distances-from-node
  [nodes source]
  (let [nodemap (into {} (map (fn [x] [(:id x) (:destinations x)]) nodes))]
    (loop [frontier #{source} visited #{} distances {} current-distance 0]
      (if (= (count visited) (count nodes))
        distances
        (let [new-visited (c-set/union frontier visited)]
          (recur
           (set (for [x (reduce concat (map #(get nodemap %) frontier)) :when (not (contains? new-visited x))] x))
           new-visited
           (reduce #(assoc %1 %2 current-distance) distances frontier)
           (inc current-distance)))))))

(defn get-distances
  [nodes]
  (reduce #(assoc %1 (:id %2) (distances-from-node nodes (:id %2))) {} nodes))

(def cost (memoize (fn
                     [nodes distances current remaining]
                     (let [flow-rates (into {} (map (fn [x] [(:id x) (:flow-rate x)]) nodes))
                           current-cost (get flow-rates current)
                           current-rate (reduce + (map #(get flow-rates %) remaining))];costs (reduce #(assoc %1 %2 (cost nodes distances %2 (disj remaining %2))) {} remaining)]
    ;(when (= (get correct-positions current) remaining) (println "Standing at" current "and the costs remaining are" costs) (println "Current rate is" current-rate))
                       (if (empty? remaining)
                         current-cost
                         (apply min (map #(+ current-cost (* current-rate (inc (get (get distances current) %))) (cost nodes distances % (disj remaining %))) remaining)))))))

(defn solve-first
  [fname]
  (let [nodes (load-to-list fname)]
    (- (* 31 (reduce + (map :flow-rate nodes))) (cost nodes (get-distances nodes) "AA" (into #{} (map :id (filter #(> (:flow-rate %) 0) nodes)))))))