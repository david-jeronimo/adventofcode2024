(ns aoc.aoc21
  (:use [aoc.lib :only [Aoc distance]]
        [clojure.string :only [split-lines]]))

(def n-keypad {\7 [0 0], \8 [1 0], \9 [2 0]
               \4 [0 1], \5 [1 1], \6 [2 1]
               \1 [0 2], \2 [1 2], \3 [2 2]
               \s [0 3] \0 [1 3], \A [2 3]})

(def d-keypad {\s [0 0] \^ [1 0], \A [2 0]
               \< [0 1], \v [1 1] \> [2 1]})

(defn group-movements [movements]
  (->> (into [\A] movements)
       (partition 2 1)
       frequencies))

(defn move [keypad target]
  (fn [[[x y :as pos] path]]
    (let [forbidden (get keypad \s)
          valid? (fn [[new-pos _]]
                   (and (not= new-pos forbidden)
                        (< (distance new-pos target) (distance pos target))))]
      (->> [[[x (dec y)] (conj path \^)]
            [[x (inc y)] (conj path \v)]
            [[(inc x) y] (conj path \>)]
            [[(dec x) y] (conj path \<)]]
           (filter valid?)))))

(defn go-to-key [keypad]
  (fn [from to]
    (let [target (get keypad to)
          start (get keypad from)]
      (letfn [(step [nodes] (mapcat (move keypad target) nodes))]
        (->> (iterate step [[start []]])
             (filter (comp #(= target %) ffirst))
             first
             (map #(-> % second (conj \A) group-movements))
             distinct)))))

(def go-to-n (memoize (go-to-key n-keypad)))
(def go-to-dir (memoize (go-to-key d-keypad)))

(def dfs
  (memoize
    (fn [num-robots level [from to]]
      (letfn [(next-comb [[[f t] n]]
                (* n (dfs num-robots (inc level) [f t])))
              (total-comb [comb]
                (->> (seq comb)
                     (transduce (map next-comb) +)))]
        (if (= level num-robots)
          1
          (->> (go-to-dir from to)
               (map total-comb)
               (apply min)))))))

(defn num-movements [num-robots combs]
  (letfn [(total-movement [[[from to] n]]
            (* n (dfs num-robots 0 [from to])))
          (total-comb [comb]
            (->> (map total-movement (seq comb))
                 (reduce +)))]
    (->> (map total-comb combs)
         (apply min))))

(defn complexity [num-robots]
  (fn [code]
    (let [length (->> (cons \A code)
                      (partition 2 1)
                      (map (fn [[a b]] (go-to-n a b)))
                      (map #(num-movements num-robots %))
                      (reduce +))
          num-code (->> (drop-while #(= \0 %) code)
                        drop-last
                        (apply str)
                        read-string)]
      (* num-code length))))

(deftype AocSol [] Aoc

  (parse-input [_ txt] (split-lines txt))

  (solution [_ part _ codes]
    (let [num-robots (case part :PartOne 2
                                :PartTwo 25)]
      (transduce (map (complexity num-robots))
                 + codes))))
