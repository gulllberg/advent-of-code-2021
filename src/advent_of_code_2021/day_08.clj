(ns advent-of-code-2021.day-08)

(def input (slurp "src/advent_of_code_2021/inputs/day08.txt"))

(defn get-four-digit-output
  [line]
  (drop 10 (re-seq #"\w+" line)))

(defn solve-a
  []
  (reduce (fn [sum line]
            (->> line
                 (get-four-digit-output)
                 (filter (fn [o]
                           (or (= (count o) 2)
                               (= (count o) 3)
                               (= (count o) 4)
                               (= (count o) 7))))
                 (count)
                 (+ sum)))
          0
          (clojure.string/split-lines input)))

(comment
  (solve-a)
  ; 514
  )
