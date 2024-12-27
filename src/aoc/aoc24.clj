(ns aoc.aoc24
  (:use [aoc.lib :only [Aoc parse-lines to-decimal-base]]))

(defn exec [wires {:keys [op w1 w2 out]}]
  (let [[wire-1 wire-2] (map #(get wires %) [w1 w2])]
    (if (and wire-1 wire-2)
      (let [result (op wire-1 wire-2)]
        (assoc wires out result))
      wires)))

(defn exec-till-done [wires gates]
  (letfn [(exec-round [wires] (reduce exec wires gates))]
    (->> (iterate exec-round wires)
         (partition 2 1)
         (some (fn [[wires-1 wires-2]] (when (= wires-1 wires-2) wires-1))))))

(def to-decimal (partial to-decimal-base 2))

(defn validate-path [gates]
  (fn [{:keys [carry]} n]
    (let [x-wire (format "x%02d" n)]
      (letfn [(by-op-and-inputs
                ([oper k1] (fn [{:keys [w1 w2 op]}]
                             (and (or (= w1 k1) (= w2 k1))
                                  (= op oper))))
                ([oper k1 k2] (fn [{:keys [w1 w2 op]}]
                                (and (= (sort [k1 k2]) (sort [w1 w2]))
                                     (= op oper)))))
              (gate ([oper input1] (->> (filter (by-op-and-inputs oper input1) gates)
                                        (some :out)))
                ([oper input1 input2] (->> (filter (by-op-and-inputs oper input1 input2) gates)
                                           (some :out))))
              (z-wire? [s] (= \z (first s)))]
        (let [xor-g1 (gate bit-xor x-wire)
              and-g1 (gate bit-and x-wire)
              xor-g2 (gate bit-xor carry xor-g1)
              and-g2 (gate bit-and carry xor-g1)
              or-g (gate bit-or and-g1 and-g2)]
          (cond
            (not carry) (let [and-g (gate bit-and x-wire)]
                          {:carry and-g})
            (not xor-g2) (let [and-g2 (gate bit-and carry and-g1)
                               or-g (gate bit-or xor-g1 and-g2)]
                           {:carry       or-g
                            :replacement [xor-g1 and-g1]})
            (not (z-wire? xor-g2)) (cond
                                     (z-wire? and-g1) (let [or-g (gate bit-or xor-g2 and-g2)]
                                                        {:carry       or-g
                                                         :replacement [xor-g2 and-g1]})
                                     (z-wire? or-g) {:carry       xor-g2
                                                     :replacement [xor-g2 or-g]}
                                     (z-wire? and-g2) (let [or-g (gate bit-or and-g1 xor-g2)]
                                                        {:carry       or-g
                                                         :replacement [xor-g2 and-g2]}))
            :else {:carry or-g}))))))

(deftype AocSol [] Aoc

  (parse-input [_ txt]
    (let [[wires [_ & gates]] (->> (parse-lines #": | -> | " identity txt)
                                   (split-with (comp not empty? first)))
          parse-gate (fn [[w1 op w2 out]] {:op (case op "AND" bit-and
                                                        "OR" bit-or
                                                        "XOR" bit-xor)
                                           :w1 w1 :w2 w2 :out out})]
      {:wires (reduce (fn [m [k v]] (assoc m k (read-string v))) {} wires)
       :gates (map parse-gate gates)}))

  (solution [_ part sample? {:keys [wires gates]}]
    (case part
      :PartOne (->> (exec-till-done wires gates)
                    (filter (comp #(= \z %) ffirst))
                    (sort-by first)
                    reverse
                    (map second)
                    to-decimal)
      :PartTwo (when-not sample?
                 (->> (reductions (validate-path gates) nil (range))
                      (mapcat :replacement)
                      (take 8)
                      sort
                      (interpose \,)
                      (apply str))))))
