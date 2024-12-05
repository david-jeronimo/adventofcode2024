(ns aoc.aoc05
  (:use [aoc.lib :only [Aoc parse-lines]]
        [clojure.math :only [floor-div]]
        [clojure.set :only [intersection]]))

(defn sort-updates [rules]
  (fn [updates]
    (let [updates-set (set updates)
          num-successors (fn [item] (-> (get rules item #{})
                                        (intersection updates-set)
                                        count))]
      (sort-by (comp - num-successors) updates))))

(defn middle-item [s]
  (->> (floor-div (count s) 2)
       (nth s)))

(deftype AocSol [] Aoc

  (parse-input [_ txt]
    (let [parse-line (fn [line] (when (seq (first line))
                                  (map read-string line)))
          [rules [_ & updates]] (->> (parse-lines #",|\|" parse-line txt)
                                     (split-with seq))]
      {:rules   (-> (reduce (fn [acc [n1 n2]] (update acc n1 conj n2)) {} rules)
                    (update-vals set))
       :updates updates}))

  (solution [_ part _ {:keys [rules updates]}]
    (letfn [(updates-to-process [original sorted]
              (let [already-sorted? (= original sorted)]
                (case part
                  :PartOne (when already-sorted? original)
                  :PartTwo (when-not already-sorted? sorted))))]
      (->> (pmap (sort-updates rules) updates)
           (map updates-to-process updates)
           (filter seq)
           (transduce (map middle-item) +)))))
