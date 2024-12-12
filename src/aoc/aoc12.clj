(ns aoc.aoc12
  (:use [aoc.lib :only [Aoc adj-4 parse-lines swap take-until]]
        [clojure.set :only [difference]]))

(defn expand [{:keys [area new-area perimeter pending]}]
  (let [neighbours (->> (mapcat adj-4 new-area)
                        (filter #(not (contains? (into area new-area) %))))
        {new-area true new-perimeter false} (group-by #(contains? pending %) neighbours)]
    {:area      (into area new-area)
     :new-area  (set new-area)
     :perimeter (into perimeter new-perimeter)
     :pending   (difference pending area)}))

(defn get-area [{:keys [pending]}]
  (let [[p & pending] pending]
    (->> (iterate expand {:new-area #{p} :area #{p} :perimeter [] :pending (set pending)})
         (filter (comp empty? :new-area))
         first)))

(defn num-distinct-lines [line]
  (->> (partition-all 2 1 line)
       (filter (fn [[a b]] (not= (inc a) b)))
       count))

(defn num-sides [points]
  (if-not points
    0
    (letfn [(num-lines [points]
              (let [[min-x max-x] (->> (map first points)
                                       ((juxt #(apply min %) #(apply max %))))
                    line-left-right (fn [x-rng inc-dec]
                                      (fn [[x y]]
                                        (and (= x-rng x)
                                             (not (contains? points [(inc-dec x) y])))))]
                (->> (for [x-rng (range min-x (inc max-x))]
                       (->> [(filter (line-left-right x-rng inc) points)
                             (filter (line-left-right x-rng dec) points)]
                            (map sort)))
                     (apply concat)
                     (filter seq)
                     (map (comp num-distinct-lines #(map second %)))
                     (reduce +))))]
      (+ (num-lines points)
         (->> points (map swap) set num-lines)))))

(defn get-area-score [part]
  (fn [area]
    (letfn [(score [{:keys [perimeter area]}]
              (case part
                :PartOne (* (count area) (count perimeter))
                :PartTwo (* (count area) (num-sides area))))]
      (->> (iterate get-area {:pending area})
           (take-until (comp empty? :pending))
           (transduce (map score) +)))))

(deftype AocSol [] Aoc

  (parse-input [_ txt]
    (let [mat (vec (parse-lines vec txt))]
      (->> (for [j (range (count mat))
                 i (range (count (first mat)))]
             [j i])
           (group-by #(get-in mat %))
           (#(update-vals % set)))))

  (solution [_ part _ garden]
    (->> (vals garden)
         (pmap (get-area-score part))
         (reduce +))))

