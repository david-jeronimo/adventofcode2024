(ns aoc.aoc02
  (:use [aoc.lib :only [Aoc parse-lines]]))

(defn all-safe? [report]
  (letfn [(inc? [[a b]] (<= (inc a) b (+ a 3)))
          (dec? [[a b]] (>= (dec a) b (- a 3)))]
    (->> (partition 2 1 report)
         ((some-fn #(every? inc? %)
                   #(every? dec? %))))))

(defn safe-removing? [report]
  (letfn [(remove [n] (into (subvec report 0 n)
                            (subvec report (inc n))))]
    (->> (range (count report))
         (map remove)
         (some all-safe?))))

(deftype AocSol [] Aoc

  (parse-input [_ txt]
    (parse-lines #" " #(mapv read-string %) txt))

  (solution [_ part _ reports]
    (let [safe? (case part :PartOne all-safe?
                           :PartTwo (some-fn all-safe? safe-removing?))]
      (-> (filter safe? reports)
          count))))
