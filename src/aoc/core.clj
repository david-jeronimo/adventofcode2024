(ns aoc.core
  (:require [aoc.lib :refer [parse-input solution]]))

(defn read-file [day suffix]
  (slurp (str "resources/aoc" day "." suffix ".txt")))

(defn aoc [day]
  (require (symbol (str "aoc.aoc" day)))
  (let [aoc-ns (find-ns (symbol (str "aoc.aoc" day)))
        aoc (ns-resolve aoc-ns (symbol "AocSol"))]
    (. aoc newInstance)))

(defn run [part day suffix]
  (let [aocDay (aoc day)
        txt (read-file day suffix)]
    (set! *warn-on-reflection* true)
    (->> (parse-input aocDay txt)
         (solution aocDay part (= suffix "sample"))
         (time))))

(defn -main [day]
  (doseq [file ["sample" "input"]
          part [:PartOne :PartTwo]]
    (->> (run part day file)
         (println part file)))
  (shutdown-agents))