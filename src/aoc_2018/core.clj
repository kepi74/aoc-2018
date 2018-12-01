(ns aoc-2018.core
    (:require [aoc-2018.days :as days]))

(defn get-final-freq
  "Day one: calculate resulting frequency"
  [freq_list]
  (reduce + freq_list))

(defn repeated-freq [freq_list]
  (reduce
    (fn [vect freq] (
        let [curr_freq (+ (last vect) freq)
            new_vect (conj vect curr_freq)]
        (if (true? (some #(= curr_freq %) vect)) (reduced (last new_vect)) new_vect)))
        ;;(if (= curr_freq 590) (reduced new_vect) new_vect)))
    [0]
    (cycle freq_list)))

(defn -main []
  (println (get-final-freq days/day_01))
  (println (repeated-freq days/day_01)))