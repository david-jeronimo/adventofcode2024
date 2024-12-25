(ns aoc.aoc25
  (:use [aoc.lib :only [Aoc parse-positions]]
        [clojure.string :only [split]]))

(defn fits? [lock key]
  (->> (map + lock key)
       (every? #(<= % 5))))

(deftype AocSol [] Aoc

  (parse-input [_ txt]
    (letfn [(parse [acc mat]
              (let [[min-max group] (if (contains? mat [0 0])
                                      [#(apply max %) :locks]
                                      [#(->> % (apply min) (- 6)) :keys])
                    add (fn [cols] (->> (map #(->> % (map second) min-max) cols)
                                        (update acc group conj)))]
                (->> (group-by first mat)
                     sort vals add)))]
      (->> (split txt #"\r\n\r\n")
           (map (comp :positions (partial parse-positions #(= \# %))))
           (reduce parse {:locks [] :keys []}))))

  (solution [_ part _ {:keys [locks keys]}]
    (case part
      :PartOne (-> (for [lock locks, key keys
                         :when (fits? lock key)]
                     [lock key])
                   count)
      :PartTwo nil)))
