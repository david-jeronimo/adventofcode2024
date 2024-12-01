(ns aoc.aoc01
  (:use [aoc.lib :only [Aoc parse-lines]]))

(deftype AocSol [] Aoc

  (parse-input [_ txt]
    (->> (parse-lines #"\s+" #(map read-string %) txt)
         (apply map vector)))

  (solution [_ part _ [left right :as lists]]
    (case part
      :PartOne (->> (map sort lists)
                    (apply map (comp abs -))
                    (reduce +))
      :PartTwo (let [right-freqs (frequencies right)
                     score (fn [n] (-> (get right-freqs n 0)
                                       (* n)))]
                 (transduce (map score) + left)))))
