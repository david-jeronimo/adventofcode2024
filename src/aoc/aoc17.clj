(ns aoc.aoc17
  (:use [aoc.lib :only [Aoc parse-lines]]
        [clojure.math :only [floor-div]]
        [clojure.math.numeric-tower :only [expt]]
        [clojure.string :only [split]]))

(defn combo [registers value]
  (letfn [(reg-val [r] (get registers r))]
    (if (<= 0 value 3)
      value
      (case value 4 (reg-val :A)
                  5 (reg-val :B)
                  6 (reg-val :C)))))

(def exec (fn [{:keys [pointer registers program output]}]
            (when-let [[op p1] (seq (nthrest program pointer))]
              (let [dv (fn [reg] (->> (combo registers p1)
                                      (expt 2)
                                      (floor-div (:A registers))
                                      (assoc registers reg)))
                    registers (case op
                                0 (dv :A)
                                1 (->> (bit-xor (:B registers) p1)
                                       (assoc registers :B))
                                2 (->> (combo registers p1)
                                       (#(rem % 8))
                                       (assoc registers :B))
                                4 (->> (bit-xor (:B registers) (:C registers))
                                       (assoc registers :B))
                                6 (dv :B)
                                7 (dv :C)
                                registers)
                    pointer (if (and (= op 3) ((complement zero?) (:A registers)))
                              p1
                              (+ 2 pointer))
                    output (if (= op 5)
                             (-> (combo registers p1) (rem 8) (#(conj output %)))
                             output)]
                {:registers registers :pointer pointer :program program :output output}))))

(defn run-program [program registers]
  (->> (iterate exec {:pointer 0 :registers registers :program program :output []})
       (take-while some?)
       (map :output)))

(deftype AocSol [] Aoc

  (parse-input [_ txt]
    (let [[registers instructions] (split txt #"\r\n\r\n")
          parse-reg (fn [acc [_ reg v]] (assoc acc (keyword reg) (read-string v)))]
      {:registers    (->> (parse-lines #": | " identity registers)
                          (reduce parse-reg {}))
       :instructions (->> (split instructions #": |,")
                          rest
                          (mapv read-string))}))

  (solution [_ part _ {:keys [registers instructions]}]
    (case part
      :PartOne (->> (run-program instructions registers)
                    last
                    (interpose ",")
                    (apply str))
      :PartTwo nil)))
