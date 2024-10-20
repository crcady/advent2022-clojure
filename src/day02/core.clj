(ns day02.core
  (:require [clojure.string :as str]))

(defn load-as-lines
  "Load a file out of the data/day01 directory as lines"
  [fname]
  (str/split (slurp (str "data/day02/" fname)) #"\n"))

(defn win-score
  "Returns 0 for a loss, 3 for a tie, 6 for a win"
  [play]
  (let [op-play (nth play 0), my-play (nth play 2)]
    (({\A {\X 3, \Y 6, \Z 0}, \B {\X 0, \Y 3, \Z 6}, \C {\X 6, \Y 0, \Z 3}} op-play) my-play)))

(defn shape-score
  "Returns 1 for Rock (X), 2 for Paper (Y), and 3 for Scissors (Z)"
  [play]
  ({\X 1, \Y 2, \Z 3} (nth play 2)))

(defn score-play
  "Score a playthrough of RPS"
  [play]
  (+ (win-score play) (shape-score play)))

(defn solve-first
  "Solve the first half ot the puzzle"
  [fname]
  (apply + (map score-play (load-as-lines fname))))

(defn convert-line
  "Converts between outcomes and plays"
  [line]
  (let [op-play (nth line 0), outcome (nth line 2)]
    (({\A {\X "A Z", \Y "A X", \Z "A Y"}, \B {\X "B X", \Y "B Y", \Z "B Z"}, \C {\X "C Y", \Y "C Z", \Z "C X"}} op-play) outcome)))

(defn solve-second
  "Solve the second half ot the puzzle"
  [fname]
  (apply + (map score-play (map convert-line (load-as-lines fname)))))