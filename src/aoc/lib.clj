(ns aoc.lib
  (:use [clojure.string :only [split split-lines]]))

(defprotocol Aoc
  (parse-input [this txt])
  (solution [this part sample? input]))

(defn parse-lines
  ([regex parse-f txt] (->> (split-lines txt)
                            (map (comp parse-f #(split % regex)))))
  ([parse-f txt] (map parse-f (split-lines txt))))

(defn matrix-positions [cell-cond mat]
  (let [parse-row (fn [j row]
                    (for [[i c] (map vector (range) row)
                          :when (cell-cond c)]
                      [i j]))]
    {:positions (->> (map-indexed parse-row mat)
                     (reduce into #{}))
     :width     (count (first mat))
     :height    (count mat)}))

(defn parse-positions [cell-cond txt]
  (->> (split-lines txt)
       (matrix-positions cell-cond)))

(def transpose #(apply map vector %))

(defn tails [coll]
  (reductions (fn [s _] (rest s)) coll coll))