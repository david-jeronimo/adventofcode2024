(ns aoc.aoc22
  (:use [aoc.lib :only [Aoc parse-lines]]
        [clojure.math :only [floor-div]]))

(defn next-secret [secret]
  (letfn [(mix-prune [n s] (-> (bit-xor n s)
                               (rem 16777216)))
          (step-1 [n] (-> (* n 64) (mix-prune n)))
          (step-2 [n] (-> (floor-div n 32) (mix-prune n)))
          (step-3 [n] (-> (* n 2048) (mix-prune n)))]
    (-> secret step-1 step-2 step-3)))

(defn secrets-seq [secret]
  (iterate next-secret secret))

(defn buyer-sequences [secret]
  (let [prices (->> (secrets-seq secret)
                    (map #(mod % 10))
                    (take 2001)
                    vec)
        buy (fn [found [i price-seq]]
              (let [hashed-seq (hash price-seq)]
                (if-not (get found hashed-seq)
                  (assoc! found hashed-seq (nth prices (+ i 4) 0))
                  found)))]
    (->> (partition 2 1 prices)
         (map (fn [[a b]] (- b a)))
         (partition 4 1)
         (map vector (range))
         (reduce buy (transient {}))
         persistent!)))

(deftype AocSol [] Aoc

  (parse-input [_ txt]
    (parse-lines read-string txt))

  (solution [_ part _ secrets]
    (case part
      :PartOne (let [secret-2000 #(-> % secrets-seq (nth 2000))]
                 (transduce (map secret-2000) + secrets))
      :PartTwo (->> (pmap buyer-sequences secrets)
                    (apply merge-with +)
                    vals
                    (reduce max)))))
