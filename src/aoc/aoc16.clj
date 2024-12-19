(ns aoc.aoc16
  (:use [aoc.lib :only [Aoc matrix-positions]]
        [aoc.lib-dijkstra :only [dijkstra]]
        [clojure.string :only [split split-lines]]))

(defn neighbours-6 [mat]
  (fn [[pos dir]]
    (let [[y x] pos
          directions (-> (if (contains? #{:E :W} dir) [:S :N] [:E :W])
                         (conj dir))
          move (fn [d] (let [next-pos (case d :E [y (inc x)]
                                              :W [y (dec x)]
                                              :N [(dec y) x]
                                              :S [(inc y) x])]
                         (when (= \. (get-in mat next-pos))
                           (if (= dir d) [[next-pos d] 1]
                                         [[pos d] 1000]))))]
      (->> (keep move directions)
           (into {})))))

(deftype AocSol [] Aoc

  (parse-input [_ txt]
    (let [mat (->> (split-lines txt)
                   (#(mapv vec %)))
          {:keys [start end]} (->> (matrix-positions #(contains? #{\S \E} %) mat)
                                   :positions
                                   (map (fn [[x y]] (if (= \S (get-in mat [y x]))
                                                      [:start [y x]]
                                                      [:end [y x]])))
                                   (into {}))]
      {:start start :end end
       :mat   (-> (assoc-in mat start \.)
                  (assoc-in end \.))}))

  (solution [_ part sample? {:keys [mat start end]}]
    (let [graph (dijkstra [start :E] (neighbours-6 mat))
          shortest-path (->> (filter (comp #(= end %) ffirst) graph)
                             (map second)
                             (apply min))]
      (case part
        :PartOne shortest-path
        :PartTwo (let [end-dir (if sample? :S :W)
                       graph-2 (dijkstra [end end-dir] (neighbours-6 mat))
                       graph-2 (update-keys graph-2
                                            (fn [[pos dir]] [pos (case dir :E :W, :W :E, :S :N, :N :S)]))]
                   (->> (merge-with + graph graph-2)
                        (filter (comp #(= % shortest-path) second))
                        (group-by ffirst)
                        count))))))

