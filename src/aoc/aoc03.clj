(ns aoc.aoc03
  (:use [aoc.lib :only [Aoc]]))

(defn exec [part]
  (fn [{:keys [enabled] :as state} {:keys [enable p1 p2]}]
    (cond (some? enable) (assoc state :enabled enable)
          (or enabled (= part :PartOne)) (update state :sum + (* p1 p2))
          :else state)))

(deftype AocSol [] Aoc

  (parse-input [_ txt]
    (let [matcher (-> (str "((?<do>(do|don't)\\(\\))"
                           "|mul\\((?<p1>\\d{1,3}),(?<p2>\\d{1,3})\\))")
                      re-pattern
                      (re-matcher txt))
          extract-inst (fn [] (when (re-find matcher)
                                (let [[d p1 p2] (map #(.group matcher ^String %) ["do" "p1" "p2"])]
                                  (case d "do()" {:enable true}
                                          "don't()" {:enable false}
                                          {:p1 (read-string p1) :p2 (read-string p2)}))))]
      (->> (repeatedly extract-inst)
           (take-while some?))))

  (solution [_ part _ program]
    (-> (reduce (exec part) {:sum 0 :enabled true} program)
        :sum)))
