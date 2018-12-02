(ns aoc-2018.core
  (:require [aoc-2018.days :as days]
            [clojure.data]))

;; Day one
;; ---------------------------------------------------------------
(defn get-final-freq
  "Day one: calculate resulting frequency"
  [freq_list]
  (reduce + freq_list))

(defn repeated-freq [freq_list]
  (reduce
   (fn [vect freq] (let [curr_freq (+ (last vect) freq)
                         new_vect (conj vect curr_freq)]
                     (if (true? (some #(= curr_freq %) vect)) (reduced (last new_vect)) new_vect)))
   [0]
   (cycle freq_list)))

;; Day two
;; ---------------------------------------------------------------
(defn count-box-letters [box_id]
  (reduce-kv
   #(assoc %1 %3 %2)
   {}
   (reduce
    (fn [m v] (merge-with + m (hash-map v 1)))
    {}
    (seq box_id))))

(defn sums-box [number box_count_letters]
  (reduce (fn [sum b] (if (contains? b number) (+ sum 1) sum)) 0 box_count_letters))

(defn box-ids-hash [box_ids]
  (let [box_count_letters (map count-box-letters box_ids)
        exactly_2 (sums-box 2 box_count_letters)
        exactly_3 (sums-box 3 box_count_letters)]
    (* exactly_2 exactly_3)))

(defn is-correct-box [word word_comp]
  (let [w_diff (reduce + (map-indexed (fn [ind itm] (if (= itm (nth word ind)) 0 1)) word_comp))]
    (if (= 1 w_diff) [word word_comp] false)))

(defn find-correct-boxs [boxs_list]
  (let [word (first boxs_list)
        w_list (rest boxs_list)
        res (remove false? (map (fn [x] (is-correct-box word x)) w_list))]
    (if (= 1 (count res)) res (find-correct-boxs w_list))))

(defn -main []
  ; (println (get-final-freq days/day_01))
  ; (println (repeated-freq days/day_01))
  (println (box-ids-hash days/day_02))
  (println (find-correct-boxs days/day_02)))