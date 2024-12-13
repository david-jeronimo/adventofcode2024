(ns aoc.aoc13
  (:use [aoc.lib :only [Aoc parse-lines]]
        [clojure.math :only [floor-div]]
        [numeric.expresso.core :only [ex solve]]))

(defn solve-1 [{:keys [a b prize]}]
  (let [[xa ya] a
        [xb yb] b
        [xp yp] prize]
    (-> (for [nb (range (inc (min 100 (floor-div xp xb) (floor-div yp yb))))
              na (range (inc (min 100 (floor-div xp xa) (floor-div yp ya))))
              :when (and (= xp (+ (* na xa) (* nb xb)))
                         (= yp (+ (* na ya) (* nb yb))))]
          (+ (* 3 na) nb))
        first)))

(defn solve-2 [{:keys [a b prize]}]
  (let [[xa ya] a
        [xb yb] b
        [xp yp] (map #(+ 10000000000000 %) prize)
        [num-a num-b] (-> (solve '[na nb]
                                 (ex (= ~xp (+ (* ~xa na)
                                               (* ~xb nb))))
                                 (ex (= ~yp (+ (* ~ya na)
                                               (* ~yb nb)))))
                          first
                          vals)]
    (when (and (int? num-a) (int? num-b))
      (+ (* 3 num-a) num-b))))

(deftype AocSol [] Aoc

  (parse-input [_ txt]
    (letfn [(parse-line [[_ x _ y]]
              (when x [(read-string x) (read-string y)]))]
      (->> (parse-lines #"\+|,|=" parse-line txt)
           (partition 3 4)
           (map #(zipmap [:a :b :prize] %)))))

  (solution [_ part _ input]
    (let [f (case part :PartOne solve-1
                       :PartTwo solve-2)]
      (->> (pmap f input)
           (transduce (filter some?) +)))))
