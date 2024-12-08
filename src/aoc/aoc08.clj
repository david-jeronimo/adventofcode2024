(ns aoc.aoc08
  (:use [aoc.lib :only [Aoc matrix-positions tails]]
        [clojure.string :only [split-lines]]))

(defn line-positions [part bounded?]
  (fn [[x y] offset-x offset-y]
    (->> (map vector (iterate offset-x x) (iterate offset-y y))
         (take-while bounded?)
         (#(case part
             :PartOne (some-> % second vector)
             :PartTwo %)))))

(defn antinodes [line-positions-f]
  (fn [antennas]
    (->> (for [[a1 & as] (tails antennas)
               a2 as
               :let [[xd yd] (map - a1 a2)]]
           (concat (line-positions-f a1 #(+ % xd) #(+ % yd))
                   (line-positions-f a2 #(- % xd) #(- % yd))))
         (apply concat))))

(deftype AocSol [] Aoc

  (parse-input [_ txt]
    (let [mat (split-lines txt)
          {:keys [positions width height]} (matrix-positions #(not= \. %) mat)]
      {:antennas (group-by (fn [[x y]] (get-in mat [y x])) positions)
       :width    width
       :height   height}))

  (solution [_ part _ {:keys [antennas height width]}]
    (letfn [(bounded? [[x y]] (and (< -1 x width)
                                   (< -1 y height)))]
      (->> (vals antennas)
           (mapcat (antinodes (line-positions part bounded?)))
           distinct count))))