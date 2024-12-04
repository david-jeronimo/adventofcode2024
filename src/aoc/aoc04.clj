(ns aoc.aoc04
  (:use [aoc.lib :only [Aoc parse-lines transpose]]))

(def xmas-str (set (map vec ["XMAS" "SAMX"])))
(def x-mas #{"MMASS" "SSAMM" "SMASM" "MSAMS"})

(defn search-row [row]
  (->> (partition 4 1 row)
       (filter #(contains? xmas-str %))
       count))

(defn x-shape? [mat]
  (let [[m1 _ s1 _ a _ m2 _ s2] (flatten mat)]
    (contains? x-mas (str m1 s1 a m2 s2))))

(defn mat-rows [mat]
  (letfn [(diagonal [mat]
            (letfn [(diag-row [offset]
                      (let [rng (range offset)]
                        (->> (map vector rng (reverse rng))
                             (keep #(get-in mat %)))))]
              (->> (range 1 (* 2 (count mat)))
                   (map diag-row))))]
    (concat mat
            (transpose mat)
            (diagonal mat)
            (-> mat reverse vec diagonal))))

(defn split-3x3 [mat]
  (let [in-threes (partial partition 3 1)]
    (->> (map in-threes mat)
         transpose
         (mapcat in-threes))))

(deftype AocSol [] Aoc

  (parse-input [_ txt]
    (vec (parse-lines vec txt)))

  (solution [_ part _ mat]
    (case part
      :PartOne (->> (mat-rows mat)
                    (transduce (map search-row) +))
      :PartTwo (->> (split-3x3 mat)
                    (filter x-shape?)
                    count))))
