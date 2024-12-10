(ns aoc.aoc10
  (:use [aoc.lib :only [Aoc adj-4 matrix-positions parse-lines swap]]))

(defn neighbours [mat]
  (fn [pos]
    (let [height (get-in mat pos)]
      (->> (adj-4 pos)
           (filter #(= (inc height) (get-in mat %)))))))

(defn score [part mat]
  (fn [pos]
    (letfn [(step [nodes] (mapcat (neighbours mat) nodes))]
      (-> (iterate step [pos])
          (nth 9)
          ((case part :PartOne distinct
                      :PartTwo identity))
          count))))

(deftype AocSol [] Aoc

  (parse-input [_ txt]
    (vec (parse-lines #(mapv (comp read-string str) %) txt)))

  (solution [_ part _ mat]
    (let [trailheads (->> (matrix-positions zero? mat)
                          :positions
                          (map swap))]
      (transduce (map (score part mat))
                 + trailheads))))
