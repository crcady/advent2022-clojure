(ns day06.core)

(defn load-as-string
  [fname]
  (slurp (str "data/day06/" fname)))

(defn to-first-packet
  "Returns the number of characters that have to be read to find four unique characters in a row"
  ([input-string]
   (to-first-packet 4 input-string))
  ([num-read current-string]
   (if (= (count (set (take 4 current-string))) 4)
     num-read
     (to-first-packet (inc num-read) (drop 1 current-string)))))

(defn to-first-message
  "Returns the number of characters that have to be read to find 14 unique characters in a row"
  ([input-string]
   (to-first-message 14 input-string))
  ([num-read current-string]
   (if (= (count (set (take 14 current-string))) 14)
     num-read
     (to-first-message (inc num-read) (drop 1 current-string)))))

(defn solve-first
  [fname]
  (to-first-packet (load-as-string fname)))

(defn solve-second
  [fname]
  (to-first-message (load-as-string fname)))