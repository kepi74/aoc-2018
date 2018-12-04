(ns aoc-2018.day-03
    (:require [clojure.java.io :as io]
              [clojure.string :as str]))

(defn parse-record
  [data]
  (hash-map :id (first data)
            :coord (map #(Integer/parseInt %) (str/split (str/replace (nth data 2) #":" "") #","))
            :dim (map #(Integer/parseInt %) (str/split (last data) #"x"))))

(defn source-to-record
  [source]
  (-> source
      (str/split #" ")
      (parse-record)))

(defn parse-source-file
  []
  (def rdr (-> "day-03.txt" io/resource io/reader))
  (map source-to-record (reduce conj [] (line-seq rdr))))

(defn create-range
  [c d]
  (range c (+ c d)))

(defn patch-points
  [patch]
  (let [[w h] (:dim patch)
        [x y] (:coord patch)]
    (for [px (range x (+ x w)) py (range y (+ y h))] (keyword(str px "x" py)))))

(defn create-patch-map
  [patch]
  (let [patch-matrix (patch-points patch)]
    (reduce (fn [patch-map coord] (merge patch-map (hash-map coord 1))) (hash-map) patch-matrix)))

(defn create-patch-ids-map
  [patch]
  (let [id (:id patch)
        patch-matrix (patch-points patch)]
    (reduce (fn [patch-map coord] (merge patch-map (hash-map coord id))) (hash-map) patch-matrix)))

(defn solution-1
  []
  (let [patches (parse-source-file)
        patches-map (map create-patch-map patches)
        patches-map-combined (reduce (fn [patches patch] (merge-with + patches patch)) (hash-map) patches-map)]
    (println (count (remove (fn [x] (= x 1)) (vals patches-map-combined))))))

(defn solution-2
  []
  (let [patches (parse-source-file)
        patches-map (map create-patch-ids-map patches)]
    (println patches-map)))