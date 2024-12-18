(ns aoc.aoc15
  (:use [aoc.lib :only [Aoc matrix-positions parse-lines swap]]
        [clojure.set :only [intersection]]
        [clojure.string :only [split split-lines]]))

(defn move [dir [x y]]
  (case dir \^ [x (dec y)]
            \v [x (inc y)]
            \< [(dec x) y]
            \> [(inc x) y]))

(defn move-robot [walls]
  (fn [{:keys [boxes robot] :as state} dir]
    (let [robot-new (move dir robot)
          move-boxes (fn [] (let [boxes-to-move (->> (iterate (partial move dir) robot-new)
                                                     (take-while #(contains? boxes %))
                                                     (map (partial move dir)))]
                              (if (contains? walls (last boxes-to-move))
                                state
                                {:boxes (-> (disj boxes robot-new)
                                            (conj (last boxes-to-move)))
                                 :robot robot-new})))]
      (cond (contains? walls robot-new) state
            (contains? boxes robot-new) (move-boxes)
            :else (assoc state :robot robot-new)))))

(defn boxes-to-move [walls boxes robot dir]
  (case dir
    \> (letfn [(expand [{:keys [added-boxes taken] :as state} pos]
                 (cond (and (contains? walls pos)
                            (or (empty? added-boxes) (not taken))) (reduced nil)
                       (contains? walls pos) (reduced added-boxes)
                       (contains? boxes pos) (-> (update state :added-boxes conj pos)
                                                 (assoc :taken true))
                       taken (assoc state :taken false)
                       :else (reduced added-boxes)))]
         (->> (iterate (partial move dir) robot)
              rest
              (reduce expand {:added-boxes [] :taken false})))
    \< (letfn [(expand [{:keys [added-boxes taken] :as state} pos]
                 (cond (and (contains? walls pos) taken) (reduced nil)
                       (contains? walls pos) (reduced added-boxes)
                       (contains? boxes pos) (-> (update state :added-boxes conj pos)
                                                 (assoc :taken true))
                       (not taken) (reduced added-boxes)
                       :else (assoc state :taken false)))]
         (->> (iterate (partial move dir) robot)
              rest
              (reduce expand {:added-boxes [] :taken true})))
    (letfn [(expand [{:keys [added-boxes positions]}]
              (if (seq (intersection walls positions))
                nil
                (let [all-positions (->> (mapcat (fn [[x y]] [[(dec x) y] [x y]]) positions)
                                         set)]
                  (if-let [next-boxes (seq (intersection boxes all-positions))]
                    {:added-boxes (into added-boxes next-boxes)
                     :positions   (->> (map (partial move dir) next-boxes)
                                       (mapcat (fn [[x y]] [[(inc x) y] [x y]]))
                                       set)}
                    added-boxes))))]
      (->> (iterate expand {:added-boxes #{} :positions #{(move dir robot)}})
           (filter #(or (nil? %) (set? %)))
           first))))

(defn move-robot-2 [walls]
  (fn [{:keys [boxes robot] :as state} dir]
    (let [robot-new (move dir robot)
          boxes-to-move (boxes-to-move walls boxes robot dir)]
      (cond
        (nil? boxes-to-move) state
        (empty? boxes-to-move) (assoc state :robot robot-new)
        :else {:boxes (-> (apply disj boxes boxes-to-move)
                          (into (map (partial move dir) boxes-to-move)))
               :robot robot-new}))))

(defn box-coord [[x y]] (+ x (* 100 y)))
(defn widen-1 [[x y]] [(* 2 x) y])
(defn widen-2 [[x y]] [[(* 2 x) y] [(inc (* 2 x)) y]])

(deftype AocSol [] Aoc

  (parse-input [_ txt]
    (let [[mat movements] (split txt #"\r\n\r\n")
          mat (->> (split-lines mat)
                   (mapv vec))
          parse-cell (fn [acc c]
                       (case (get-in mat (swap c))
                         \# (update acc :walls conj c)
                         \O (update acc :boxes conj c)
                         \@ (assoc acc :robot c)))
          parse-movement (fn [c] c)]
      (->> (matrix-positions #(not= \. %) mat)
           :positions
           (reduce parse-cell {:walls #{} :boxes #{}})
           (#(assoc % :movements (->> (parse-lines (partial map parse-movement) movements)
                                      (apply concat)))))))

  (solution [_ part _ {:keys [walls robot boxes movements]}]

    (let [[f walls boxes robot] (case part :PartOne [move-robot walls boxes robot]
                                           :PartTwo [move-robot-2
                                                     (set (mapcat widen-2 walls))
                                                     (set (map widen-1 boxes))
                                                     (widen-1 robot)])]
      (->> (reduce (f walls) {:boxes boxes :robot robot} movements)
           :boxes
           (transduce (map box-coord) +)))))
