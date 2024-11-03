(ns day11.core
  (:require [clojure.string :as str]))

(defn make-map
  "Expects a big string describing a monkey. Returns a map with the relevant info."
  [monkey]
  (let [[name-line items-line operation-line test-line true-line false-line] (str/split-lines monkey)
        monkey-id (re-find #"\d" name-line)
        monkey-items (re-seq #"\d+" items-line)
        [_ op operand] (re-find #"old ([*+]) ([\d\w]+)" operation-line)
        divisor (re-find #"\d+" test-line)
        true-target (re-find #"\d" true-line)
        false-target (re-find #"\d" false-line)]
    {:id (parse-long monkey-id)
     :items (into [] (map parse-long monkey-items))
     :operation (eval (read-string (str "(fn [old] (" op " old " operand "))")))
     :divisor (parse-long divisor)
     :true-target (parse-long true-target)
     :false-target (parse-long false-target)}))

(defn throw-fn
  "Returns a function that accepts a single item that a monkey throws. That function returns a (item, destination) pair."
  [monkey-map]
  (fn [item]
    (let [worry
          (quot ((:operation monkey-map) item) 3)]
      (if (= 0 (mod worry (:divisor monkey-map)))
        (list worry (:true-target monkey-map))
        (list worry (:false-target monkey-map))))))

(defn throw-fn2
  "Returns a function that accepts a single item that a monkey throws. That function returns a (item, destination) pair."
  [monkey-map product-of-primes]
  (fn [item]
    (let [worry
          ((:operation monkey-map) item)]
      (if (= 0 (mod worry (:divisor monkey-map)))
        (list (mod worry product-of-primes) (:true-target monkey-map))
        (list (mod worry product-of-primes) (:false-target monkey-map))))))

(defn initial-one
  ([monkeys]
   (initial-one {} monkeys))
  ([current remaining]
   (if (empty? remaining)
     current
     (let [current-map (make-map (first remaining))]
       (recur
        (assoc current (:id current-map) {:throw (throw-fn current-map)
                                          :items (:items current-map)
                                          :activity 0})
        (rest remaining))))))

(defn initial-two
  ([monkeys product-of-primes]
   (initial-two {} monkeys product-of-primes))
  ([current remaining product-of-primes]
   (if (empty? remaining)
     current
     (let [current-map (make-map (first remaining))]
       (recur
        (assoc current (:id current-map) {:throw (throw-fn2 current-map product-of-primes)
                                          :items (:items current-map)
                                          :activity 0})
        (rest remaining)
        product-of-primes)))))

(defn apply-moves
  [monkey-map moves]
  (if (empty? moves)
    monkey-map
    (let [[item destination] (first moves)
          target-monkey (get monkey-map destination)]
      (recur (assoc monkey-map destination (assoc target-monkey :items (conj (:items target-monkey) item))) (rest moves)))))

(defn turn
  [monkey-map monkey]
  (let [current-map (get monkey-map monkey)]
    (assoc (apply-moves monkey-map (map (:throw current-map) (:items current-map)))
           monkey
           (assoc current-map :items [] :activity (+ (count (:items current-map)) (:activity current-map))))))

(defn round
  [monkey-map]
  (reduce turn monkey-map (range (count monkey-map))))

(defn rounds
  [monkey-map remaining]
  (if (zero? remaining)
    monkey-map
    (recur (round monkey-map) (dec remaining))))

(defn solve-first
  [fname]
  (apply * (take 2 (reverse (sort (map #(:activity (second %)) (rounds (initial-one (str/split (slurp (str "data/day11/" fname)) #"\n\n")) 20)))))))

(defn solve-second
  [fname]
  (let [list-of-strings (str/split (slurp (str "data/day11/" fname)) #"\n\n")
        product-of-primes (reduce * (map :divisor (map make-map list-of-strings)))]
    (println product-of-primes)
    (apply * (take 2 (reverse (sort (map #(:activity (second %)) (rounds (initial-two list-of-strings product-of-primes) 10000))))))))