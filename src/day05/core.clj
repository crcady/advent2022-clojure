(ns day05.core
  (:require [clojure.string :as str]))

(defn load-as-lines
  "Load a file out of the data/day05 directory as lines"
  [fname]
  (str/split (slurp (str "data/day05/" fname)) #"\n"))

(defn move
  [current src dst]
  (let [temp (first (current src))]
    (-> current
        (assoc src (drop 1 (current src)))
        (assoc dst (cons temp (current dst))))))

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
  (let [initial-state {"1" '(\N \Z), "2" '(\D \C \M), "3" '(\P)}
        function-map {:mov move}
        instructions '((:mov ["2" "1"])
                       (:mov ["1" "3"])
                       (:mov ["1" "3"])
                       (:mov ["1" "3"])
                       (:mov ["2" "1"])
                       (:mov ["2" "1"])
                       (:mov ["1" "2"]))]
    (run-machine initial-state function-map instructions)))
