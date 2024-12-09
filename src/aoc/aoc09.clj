(ns aoc.aoc09
  (:use [aoc.lib :only [Aoc]]))

(defn defragment [disk-map]
  (let [disk (->> (map vector disk-map (cycle [true false]))
                  (map-indexed (fn [i [n filled?]]
                                 (if filled? (repeat n (/ i 2))
                                             (repeat n nil))))
                  flatten)
        files (vec (filter some? disk))
        add (fn [[_ remaining] n]
              (if n [n remaining]
                    [(peek remaining) (pop remaining)]))]
    (->> (reductions add [nil files] disk)
         rest
         (take (count files)))))

(defn fill-in-2 [{:keys [files gaps] :as disk-map} file-id]
  (let [{file-pos :pos width :width} (get files file-id)
        [before [{gap-pos :pos gap-width :width} & after]] (split-with #(< (:width %) width) gaps)]
    (cond (or (nil? gap-pos) (> gap-pos file-pos)) disk-map
          (= gap-width width) (-> (assoc-in disk-map [:files file-id :pos] gap-pos)
                                  (assoc :gaps (concat before after)))
          (> gap-width width) (-> (assoc-in disk-map [:files file-id :pos] gap-pos)
                                  (assoc :gaps (concat before [{:pos   (+ gap-pos width)
                                                                :width (- gap-width width)}] after))))))
(defn check-sum-2 [[id {:keys [pos width]}]]
  (->> (range pos (+ pos width))
       (transduce (map #(* id %)) +)))

(defn build-map [disk-map]
  (letfn [(add [{:keys [offset id] :as state} [n filled-in?]]
            (let [state (if filled-in?
                          (-> (assoc-in state [:files id] {:pos offset :width n})
                              (update :id inc))
                          (update state :gaps conj {:pos offset :width n}))]
              (update state :offset + n)))]
    (->> (map vector disk-map (cycle [true false]))
         (reduce add {:files {} :gaps [] :offset 0 :id 0})
         (#(dissoc % :id :offset)))))

(deftype AocSol [] Aoc

  (parse-input [_ txt]
    (map (comp read-string str) txt))

  (solution [_ part _ disk-map]
    (case part
      :PartOne (->> (defragment disk-map)
                    (map-indexed #(* %1 (first %2)))
                    (reduce +))
      :PartTwo (let [disk-map (build-map disk-map)
                     {:keys [files]} disk-map
                     file-ids (sort-by - (keys files))]
                 (->> (reduce fill-in-2 disk-map file-ids)
                      :files
                      (transduce (map check-sum-2) +))))))
