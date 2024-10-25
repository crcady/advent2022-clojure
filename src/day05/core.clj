(ns day05.core
  (:require [clojure.string :as str]))

(defn load-as-lines
  "Load a file out of the data/day05 directory as lines"
  [fname]
  (str/split (slurp (str "data/day05/" fname)) #"\n"))

(defn load-as-sections
  "Load the file in two section. The first one is reversed so that it can be handled bottom-up."
  [fname]
  (let [sections (str/split (slurp (str "data/day05/" fname)) #"\n\n")]
    {:initial (reverse (str/split (first sections) #"\n"))
     :instructions (str/split (second sections) #"\n")}))

(defn line-to-moves
  "Parse a line such as 'move 1 from 2 to 1 to a list of instructions"
  [line]
  (let [[count src dst] (re-seq #"\d+" line)]
    (repeat (parse-long count) (list :mov [src dst]))))

(defn line-to-one-move
  "Parse a line into ((:mov [cnt src dst]))"
  [line]
  (let [[cnt src dst] (re-seq #"\d+" line)]
    (list (list :mov [(parse-long cnt) src dst]))))


(defn move-list
  "Parse the instructions section of the input into a list of :mov instructions"
  [read-fn instructions]
  (reduce concat () (map read-fn instructions)))

(defn state-from-initial
  "Given the first line of the :initial section, produce an initial state"
  [line]
  (reduce #(assoc %1 %2 ()) {} (re-seq #"\d" line)))

(defn line-to-puts
  "Given a line of the :initial ascii art, return a list of :put instructions"
  [line]
  (->> line
       (re-seq #"....?"); Slice it into 3 or 4 character sequences
       (map #(list (inc %1) (re-find #"\w" %2)) (range)) ;a sequence of tuples
       (filter second) ; toss the empty identifiers
       (map #(list :put [(second %) (str (first %))]))))

(defn puts-list
  "Parse the ascii-art into a list of :put instructions"
  [lines]
  (reduce concat () (map line-to-puts lines)))

(defn move
  "Moves a symbol from src stack to dst stack"
  ([current src dst]
   (move current 1 src dst))
  ([current cnt src dst]
   (let [temp (take cnt (current src))]
     (-> current
         (assoc src (drop cnt (current src)))
         (assoc dst (concat temp (current dst)))))))

(defn put
  "Puts symbol sym on stack dst"
  [current sym dst]
  (assoc current dst (cons sym (current dst))))

(defn step-machine
  "Given a machine definition and an instruction, return the next machine"
  [machine ins]
  (let [[op args] ins
        functions (machine :functions)
        fun (functions op)
        current (machine :state)]
    {:functions functions :state (apply fun current args)}))

(defn run-machine
  "Run a machine with initial state, through function-map, by instructions"
  [initial, fmap, instrs]
  (reduce step-machine {:state initial :functions fmap} instrs))

(defn manually-run-example
  "Manually run the example through the machine"
  []
  (let [initial-state {"1" (), "2" (), "3" ()}
        function-map {:mov move :put put}

        instructions '((:put ["Z" "1"])
                       (:put ["M" "2"])
                       (:put ["P" "3"])
                       (:put ["N" "1"])
                       (:put ["C" "2"])
                       (:put ["D" "2"])
                       (:mov ["2" "1"])
                       (:mov ["1" "3"])
                       (:mov ["1" "3"])
                       (:mov ["1" "3"])
                       (:mov ["2" "1"])
                       (:mov ["2" "1"])
                       (:mov ["1" "2"]))]
    (run-machine initial-state function-map instructions)))

(defn get-answer
  "Nicely prints the answer given a state"
  [state]
  (let [num-cols (count state)]
    (reduce str (map #(first (get state (str %))) (map inc (range num-cols))))))


(defn solve-first
  "Solve the first half of the challenge for a given file"
  [fname]
  (let [sections (load-as-sections fname)
        initial-state (state-from-initial (first (:initial sections)))
        function-map {:mov move :put put}
        instructions (concat (puts-list (drop 1 (:initial sections))) (move-list line-to-moves (:instructions sections)))]
    (get-answer (:state (run-machine initial-state function-map instructions)))))

(defn solve-second
  "Solve the second half of the challenge for a given file"
  [fname]
  (let [sections (load-as-sections fname)
        initial-state (state-from-initial (first (:initial sections)))
        function-map {:mov move :put put}
        instructions (concat (puts-list (drop 1 (:initial sections))) (move-list line-to-one-move (:instructions sections)))]
    (get-answer (:state (run-machine initial-state function-map instructions)))))