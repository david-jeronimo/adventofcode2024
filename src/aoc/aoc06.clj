(ns aoc.aoc06
  (:use [aoc.lib :only [Aoc parse-positions]]
        [clojure.set :only [difference]]))

(defn move [[x y] dir]
  (case dir :U [x (dec y)]
            :D [x (inc y)]
            :L [(dec x) y]
            :R [(inc x) y]))

(defn turn-right [dir] (case dir :U :R, :R :D, :D :L, :L :U))

(defn step [obstacles {:keys [pos dir] :as guard}]
  (let [next-pos (move pos dir)]
    (if (contains? obstacles next-pos)
      (update guard :dir turn-right)
      (assoc guard :pos next-pos))))

(defn cycle? [obstacles bounded? guard]
  (loop [visited (transient #{}) {:keys [pos] :as guard} guard]
    (cond (contains? visited guard) true
          (not (bounded? pos)) false
          :else (recur (conj! visited guard) (step obstacles guard)))))

(deftype AocSol [] Aoc

  (parse-input [_ txt]
    (let [{:keys [positions width height]} (parse-positions #(= \# %) txt)
          start (-> (parse-positions #(= \^ %) txt) :positions first)]
      {:obstacles positions :width width :height height :start start}))

  (solution [_ part _ {:keys [obstacles width height start]}]
    (let [bounded? (fn [[x y]] (and (< -1 x width)
                                    (< -1 y height)))
          init {:pos start :dir :U}
          guard-states (->> (iterate (partial step obstacles) init)
                            (take-while (comp bounded? :pos)))]
      (case part
        :PartOne (->> (map :pos guard-states)
                      distinct count)
        :PartTwo (let [move-ahead (fn [{:keys [pos dir]}] (move pos dir))
                       candidates (-> (map move-ahead guard-states)
                                      set
                                      (difference obstacles))
                       add-obstacle (fn [obstacle] (-> (conj obstacles obstacle)
                                                       (cycle? bounded? init)))]
                   (->> (pmap add-obstacle candidates)
                        (filter identity)
                        count))))))
