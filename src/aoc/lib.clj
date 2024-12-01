(ns aoc.lib
  (:use [clojure.string :only [split split-lines]]))

(defprotocol Aoc
  (parse-input [this txt])
  (solution [this part sample? input]))

(defn parse-lines [regex parse-f txt]
  (->> (split-lines txt)
       (map (comp parse-f #(split % regex)))))