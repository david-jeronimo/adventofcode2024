(ns aoc.aoc14
  (:use [aoc.lib :only [Aoc parse-lines]]
        [clojure.math :only [floor-div]]))

(defn tick-robot [width height seconds {[x y] :pos [vx vy] :v}]
  (let [[vx vy] [(if (pos? vx) vx (+ width vx))
                 (if (pos? vy) vy (+ height vy))]]
    [(-> (+ x (* seconds vx)) (rem width))
     (-> (+ y (* seconds vy)) (rem height))]))

(defn quadrant [width height [x y]]
  (let [[qx qy] [(cond (<= 0 x (dec (floor-div width 2))) 0
                       (< (floor-div width 2) x width) 1)
                 (cond (<= 0 y (dec (floor-div height 2))) 0
                       (< (floor-div height 2) y height) 1)]]
    (when (and qx qy) [qx qy])))

(deftype AocSol [] Aoc

  (parse-input [_ txt]
    (letfn [(parse [[_ x y vx vy]]
              {:pos [x y] :v [vx vy]})]
      (parse-lines #"=|,| v=|," (comp parse #(map read-string %)) txt)))

  (solution [_ part sample? robots]
    (let [[width height] (if sample? [11 7] [101 103])
          tick (fn [seconds] (partial tick-robot width height seconds))]
      (case part
        :PartOne (->> (keep #(->> % ((tick 100)) (quadrant width height)) robots)
                      frequencies vals
                      (reduce *))
        :PartTwo (when-not sample?
                   (->> (range)
                        (map #(map (tick %) robots))
                        (take-while (complement (partial apply distinct?)))
                        count))))))
