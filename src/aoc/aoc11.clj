(ns aoc.aoc11
  (:use [aoc.lib :only [Aoc]]
        [clojure.math :only [floor-div]]
        [clojure.string :only [split]]))

(defn split-in-two [stone-str]
  (->> (split-at (floor-div (count stone-str) 2) stone-str)
       (map #(->> % (apply str) (. Long parseLong)))))

(defn blink-stone [[stone num-stones]]
  (let [stone-str (str stone)
        num-digits (count stone-str)]
    (cond (zero? stone) {1 num-stones}
          (even? num-digits) (let [[a b] (split-in-two stone-str)]
                               (if (= a b) {a (* 2 num-stones)}
                                           {a num-stones, b num-stones}))
          :else {(* stone 2024) num-stones})))

(defn blink [stones]
  (->> (map blink-stone stones)
       (apply merge-with +)))

(deftype AocSol [] Aoc

  (parse-input [_ txt]
    (->> (split txt #" ")
         (map read-string)
         frequencies))

  (solution [_ part _ stones]
    (let [num-blinks (case part :PartOne 25
                                :PartTwo 75)]
      (->> (iterate blink stones)
           (#(nth % num-blinks))
           vals
           (reduce +)))))
