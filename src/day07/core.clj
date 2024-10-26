(ns day07.core
  (:require [clojure.string :as str]))

(defn load-as-lines
  "Load a file out of the data/day07 directory as lines"
  [fname]
  (str/split (slurp (str "data/day07/" fname)) #"\n"))

(defn process-cd
  [state line]
  (let [pwd (:pwd state)]
    (if-some [[_ dst] (re-matches #"\$\h+cd\h+(.+)" line)]
      (if (= dst "..")
        (assoc state :pwd (drop 2 pwd))
        (assoc state :pwd (concat (list "/" dst) pwd)))
      state)))

(defn process-dir
  [state line]
  (let [fs (:fs state) pwd (:pwd state)]
    (if-some [[_ fname] (re-matches #"dir\h+(.+)" line)]
      (assoc state :fs (cons {:type "directory" :name fname :path (apply str (reverse pwd))} fs))
      state)))

(defn process-file
  [state line]
  (let [fs (:fs state) pwd (:pwd state)]
    (if-some [[_ size fname] (re-matches #"(\d+)\h+(.+)" line)]
      (assoc state :fs (cons {:type "file" :size (parse-long size) :name fname :path (apply str (reverse pwd))} fs))
      state)))

(defn get-size
  [fs obj]
  (if (= (:type obj) "file")
    (:size obj)
    (let [children (filter #(= (:path %) (str (:path obj) (:name obj) "/")) fs)]
      (reduce + (map (partial get-size fs) children)))))

(defn solve-first
  [fname]
  (let [fs (:fs (reduce #(-> %1 (process-cd %2) (process-dir %2) (process-file %2)) {:fs () :pwd ()} (load-as-lines fname)))
        dirs (filter #(= (:type %) "directory") fs)]
    (->> dirs
         (map (partial get-size fs))
         (filter #(<= %1 100000))
         (reduce +))))

(defn solve-second
  [fname]
  (let [fs (:fs (reduce #(-> %1 (process-cd %2) (process-dir %2) (process-file %2)) {:fs () :pwd ()} (load-as-lines fname)))
        dirs (filter #(= (:type %) "directory") fs)
        root-size (get-size fs {:type "directory" :name "" :path "/"})]
    (->> dirs
         (map (partial get-size fs))
         (filter #(>= %1 (- root-size 40000000)))
         (reduce min))))