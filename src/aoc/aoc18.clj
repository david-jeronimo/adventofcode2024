(ns aoc.aoc18
  (:use [aoc.lib :only [Aoc adj-4 parse-lines]]
        [clojure.set :only [difference]]))

(defn steps [bounded? falling-bytes]
  (fn [{:keys [visited nodes]}]
    (let [neighbours (->> (mapcat adj-4 nodes)
                          (filter bounded?)
                          set
                          (#(difference % falling-bytes)))]
      {:visited (into visited nodes)
       :nodes   (difference neighbours visited)})))

(defn bfs [falling-bytes num-bytes width]
  (let [bounded? (fn [pos] (every? #(<= 0 % width) pos))
        falling-bytes (set (take num-bytes falling-bytes))]
    (->> {:nodes [[0 0]] :visited #{}}
         (iterate (steps bounded? falling-bytes))
         (map :nodes))))

(defn exit-found? [width falling-bytes]
  (fn [num-bytes]
    (->> (bfs falling-bytes num-bytes width)
         (keep #(cond (empty? %) false
                      (contains? % [width width]) true))
         first)))

(deftype AocSol [] Aoc

  (parse-input [_ txt]
    (parse-lines #"," #(mapv read-string %) txt))

  (solution [_ part sample? falling-bytes]
    (let [width (if sample? 6 70)]
      (case part
        :PartOne (let [num-bytes (if sample? 12 1024)]
                   (->> (bfs falling-bytes num-bytes width)
                        (take-while #(not (contains? % [width width])))
                        count))
        :PartTwo (->> (range (count falling-bytes) 0 -1)
                      (filter (exit-found? width falling-bytes))
                      first
                      (nth falling-bytes)
                      ((fn [[x y]] (str x \, y))))))))

