(ns aoc.aoc20
  (:use [aoc.lib :only [Aoc distance matrix-positions]]
        [aoc.lib-dijkstra :only [dijkstra]]
        [clojure.string :only [split-lines]]))

(def adj-4
  (fn [bounded? n [x-pos y-pos]]
    (for [x (range (- x-pos n) (+ x-pos n 1))
          y (range (- y-pos n) (+ y-pos n 1))
          :when (and (bounded? [x y])
                     (<= 1 (distance [x y] [x-pos y-pos]) n))]
      [x y])))

(defn neighbours [bounded? open]
  (fn [pos]
    (->> (adj-4 bounded? 1 pos)
         (filter #(contains? open %))
         (map #(-> [% 1]))
         (into {}))))

(defn gains [bounded? graph-rev min-saved dist]
  (fn [pos]
    (let [from-rev (get graph-rev pos)
          saving? (fn [new-pos]
                    (when-let [to-rev (get graph-rev new-pos)]
                      (when (<= min-saved (- from-rev to-rev (distance pos new-pos)))
                        [pos new-pos])))]
      (->> (adj-4 bounded? dist pos)
           (keep saving?)))))

(deftype AocSol [] Aoc

  (parse-input [_ txt]
    (let [mat (->> (split-lines txt)
                   (#(mapv vec %)))
          parse-tile (fn [acc [x y]]
                       (case (get-in mat [y x])
                         \S (assoc acc :start [x y])
                         \E (assoc acc :end [x y])
                         (update acc :open conj [x y])))
          {:keys [positions width]} (matrix-positions #(contains? #{\. \S \E} %) mat)
          {:keys [start end open]} (reduce parse-tile {:open #{}} positions)]
      {:start start :end end :open (into open [start end]) :width width}))

  (solution [_ part sample? {:keys [end open width]}]
    (let [bounded? (fn [pos] (every? #(< 0 % (dec width)) pos))
          graph-rev (dijkstra end (neighbours bounded? open))
          min-savings (if sample? 12 100)
          dist (case part :PartOne 2
                          :PartTwo 20)]
      (->> (mapcat (gains bounded? graph-rev min-savings dist) open)
           distinct
           count))))

