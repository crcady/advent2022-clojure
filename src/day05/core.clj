(ns day05.core
  (:require [clojure.string :as str]))

(defn load-as-lines
  "Load a file out of the data/day05 directory as lines"
  [fname]
  (str/split (slurp (str "data/day05/" fname)) #"\n"))

(defn move
  "Moves a symbol from src stack to dst stack"
  [current src dst]
  ; Not safe if src or dst don't exist in the state. Src must exist, but dst?
  (let [temp (first (current src))]
    (-> current
        (assoc src (drop 1 (current src)))
        (assoc dst (cons temp (current dst))))))

(defn put
  "Puts symbol sym on stack dst"
  [current sym dst]
  ; This isn't safe when dst isn't already in the state
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
        instructions '((:put [\Z "1"])
                       (:put [\M "2"])
                       (:put [\P "3"])
                       (:put [\N "1"])
                       (:put [\C "2"])
                       (:put [\D "2"])
                       (:mov ["2" "1"])
                       (:mov ["1" "3"])
                       (:mov ["1" "3"])
                       (:mov ["1" "3"])
                       (:mov ["2" "1"])
                       (:mov ["2" "1"])
                       (:mov ["1" "2"]))]
    (run-machine initial-state function-map instructions)))
