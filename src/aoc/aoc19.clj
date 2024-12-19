(ns aoc.aoc19
  (:use [aoc.lib :only [Aoc parse-lines]]))

(defn inits [s]
  (map #(take (inc %) s) (range (count s))))

(defn combinations [patterns s]
  (letfn [(match [segment i]
            (when (-> (get patterns i) (contains? (vec segment)))
              i))]
    (->> (range 1 (inc (count s)))
         (map match (inits s))
         (filter some?))))

(defn combs-rest [patterns max-segment]
  (fn [[s num-combs]]
    (let [segment (take max-segment s)]
      (->> (combinations patterns segment)
           (map #(if-let [rest-s (seq (drop % s))]
                   [rest-s num-combs]
                   [:done num-combs]))))))

(defn step [patterns max-segment]
  (fn [nodes]
    (letfn [(add-combs [m] (transduce (map second) + m))]
      (->> (filter (comp #(not= :done %) first) nodes)
           (mapcat (combs-rest patterns max-segment))
           (group-by #(if (= :done %) % (first %)))
           (#(update-vals % add-combs))))))

(defn total-combs [patterns max-segment]
  (fn [towel]
    (->> (iterate (step patterns max-segment) [[towel 1]])
         (take-while seq)
         (apply concat)
         (filter (comp #(= :done %) first))
         (map second))))

(deftype AocSol [] Aoc

  (parse-input [_ txt]
    (let [[patterns _ & towels] (parse-lines #", " identity txt)]
      {:patterns (->> (map vec patterns) (group-by count) (#(update-vals % set)))
       :towels   (->> (flatten towels) (map vec))}))

  (solution [_ part _ {:keys [patterns towels]}]
    (let [max-segment (apply max (keys patterns))
          total-numbers (total-combs patterns max-segment)]
      (case part
        :PartOne (-> (filter (comp seq total-numbers) towels)
                     count)
        :PartTwo (->> (mapcat total-numbers towels)
                      (reduce +))))))

