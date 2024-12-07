(ns aoc.aoc07
  (:use [aoc.lib :only [Aoc parse-lines]]))

(defn valid? [operators]
  (fn [{:keys [test numbers]}]
    (let [[n1 & rest-numbers] numbers
          add-number (fn [results n]
                       (for [op operators
                             result results
                             :let [res (op result n)]
                             :when (<= res test)]
                         res))]
      (->> (reduce add-number [n1] rest-numbers)
           (some #(when (= test %) test))))))

(defn concatenate [a b] (-> (str a b) read-string))

(deftype AocSol [] Aoc

  (parse-input [_ txt]
    (letfn [(parse [[test & numbers]]
              (zipmap [:test :numbers] [test numbers]))]
      (parse-lines #": | " (comp parse #(map read-string %)) txt)))

  (solution [_ part _ equations]
    (let [operators (case part :PartOne [+ *]
                               :PartTwo [+ * concatenate])]
      (->> (pmap (valid? operators) equations)
           (transduce (filter some?) +)))))
