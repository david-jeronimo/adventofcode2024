(ns aoc.aoc23
  (:use [aoc.lib :only [Aoc parse-lines]]
        [clojure.set :only [intersection]]))

(defn expand-groups [connections]
  (fn [groups]
    (letfn [(expand-group [group]
              (->> (map #(get connections %) group)
                   (apply intersection)
                   (map #(conj group %))))]
      (-> (mapcat expand-group groups)
          distinct))))

(defn password [computers]
  (->> (sort computers)
       (interpose ",")
       (apply str)))

(deftype AocSol [] Aoc

  (parse-input [_ txt]
    (letfn [(add-line [m [k v]]
              (-> (update m k conj v)
                  (update v conj k)))]
      (->> (parse-lines #"-" identity txt)
           (reduce add-line {})
           (#(update-vals % set)))))

  (solution [_ part _ connections]
    (case part
      :PartOne (->> (keys connections)
                    (filter (comp #(= \t %) first))
                    (map #(-> #{%}))
                    (iterate (expand-groups connections))
                    (#(nth % 2))
                    count)
      :PartTwo (->> (keys connections)
                    (map #(-> #{%}))
                    (iterate (expand-groups connections))
                    (some #(when (= 1 (count %))
                             (first %)))
                    password))))
