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

(defn adj-4 [pos]
  (map (partial mapv + pos) [[-1 0] [0 -1] [0 1] [1 0]]))

(defn swap [[a b]] [b a])

(def transpose #(apply map vector %))

(defn tails [coll]
  (reductions (fn [s _] (rest s)) coll coll))

(defn take-until [pred coll]
  (lazy-seq
    (when-let [[f & r] (seq coll)]
      (if (pred f)
        [f]
        (cons f (take-until pred r))))))

(defn distance [p1 p2]
  (->> (map (comp abs -) p1 p2)
       (apply +)))
