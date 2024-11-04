(ns day12.core)

(defn mk-node
  "Helper function to make a map with some default values"
  ([id x y z]
   (mk-node id x y z false false))
  ([id x y z start end]
   {:id id :x x :y y :z z :start start :end end}))

(defn load-elevations
  "Parses single string into a list of x/y/z coordinates plus some metadata"
  ([input]
   (load-elevations {:nodes () :x 0 :y 0} input))
  ([current remaining]
   (if (empty? remaining)
     (:nodes current)
     (let [next (first remaining) x (:x current) y (:y current) nodes (:nodes current)]
       (case next
         \newline (recur (assoc current :y (inc y) :x 0) (rest remaining))
         \S (recur (assoc current :x (inc x) :nodes (cons (mk-node (count nodes) x y 0 true false) nodes)) (rest remaining))
         \E (recur (assoc current :x (inc x) :nodes (cons (mk-node (count nodes) x y 25 false true) nodes)) (rest remaining))
         (recur (assoc current :x (inc x) :nodes (cons (mk-node (count nodes) x y (- (int next) 97)) nodes)) (rest remaining)))))))

(defn neighbors
  "Returns the (up to four) adjacent nodes"
  [nodes node]
  (filter #(= 1 (+ (abs (- (:x node) (:x %))) (abs (- (:y node) (:y %))))) nodes))

(defn reachability-map
  "Returns a map of id -> list [id] based on only climbing up one level at a time"
  ([elevations] (reachability-map elevations {} (range (count elevations))))
  ([elevations current remaining]
   (if (empty? remaining)
     current
     (let [current-node (first (filter #(= (:id %) (first remaining)) elevations))]
       (recur elevations (assoc current (:id current-node) (map :id (filter #(<= (:z %) (inc (:z current-node))) (neighbors elevations current-node)))) (rest remaining))))))

(defn reachability-map2
  "Returns a map id-> list[id] based on reversed logic"
  ([elevations] (reachability-map2 elevations {} (range (count elevations))))
  ([elevations current remaining]
   (if (empty? remaining)
     current
     (let [current-node (first (filter #(= (:id %) (first remaining)) elevations))]
       (recur elevations (assoc current (:id current-node) (map :id (filter #(>= (:z %) (dec (:z current-node))) (neighbors elevations current-node)))) (rest remaining))))))

(defn get-distances
  "Poor implementation of Djikstra's algorithm. Returns a map of id -> distance (both ints)"
  ([edges start] (get-distances edges {} (into {} (map (fn [[id _]] [id (if (= id start) 0 9999)]) edges))))
  ([edges visited remaining]
   (if (empty? remaining)
     visited
     (let [[next-id next-distance] (first (sort-by second remaining))
           to-update (filter #(> (get remaining %) next-distance) (filter #(contains? remaining %) (get edges next-id)))]
       (recur edges (assoc visited next-id next-distance) (reduce #(assoc %1 %2 (inc next-distance)) (dissoc remaining next-id) to-update))))))

(defn solve-first
  [fname]
  (let [elevations (load-elevations (slurp (str "data/day12/" fname)))
        start-index (:id (first (filter #(true? (:start %)) elevations)))
        end-index (:id (first (filter #(true? (:end %)) elevations)))]
    (get (get-distances (reachability-map elevations) start-index) end-index)))


(defn solve-second
  [fname]
  (let [elevations (load-elevations (slurp (str "data/day12/" fname)))
        start-index (:id (first (filter #(true? (:end %)) elevations)))
        possible-ends (map :id (filter #(= (:z %) 0) elevations))
        distances (get-distances (reachability-map2 elevations) start-index)]
    (reduce min (map #(get distances %) possible-ends))))