(ns advent-of-code-2021.day-04
  (:require [ysera.test :refer [is= is is-not]]))

(def input-numbers "31,88,35,24,46,48,95,42,18,43,71,32,92,62,97,63,50,2,60,58,74,66,15,87,57,34,14,3,54,93,75,22,45,10,56,12,83,30,8,76,1,78,82,39,98,37,19,26,81,64,55,41,16,4,72,5,52,80,84,67,21,86,23,91,0,68,36,13,44,20,69,40,90,96,27,77,38,49,94,47,9,65,28,59,79,6,29,61,53,11,17,73,99,25,89,51,7,33,85,70")
(def input-boards (slurp "src/advent_of_code_2021/inputs/day04.txt"))

(defn board-string->board
  [board-string]
  (let [rows (map (fn [row-string]
                    (map (fn [p]
                           (read-string (apply str p)))
                         (partition 2 3 row-string)))
                  (clojure.string/split-lines board-string))
        columns (reduce (fn [columns i]
                          (conj columns (map (fn [row]
                                               (nth row i))
                                             rows)))
                        []
                        (range 5))]
    (concat rows columns)))

(defn filter-number
  [board number]
  (map (fn [row-or-column]
         (remove (fn [n]
                   (= n number))
                 row-or-column))
       board))

;, https://stackoverflow.com/questions/10192602/return-first-item-in-a-map-list-sequence-that-satisfies-a-predicate
(defn find-first
  [pred coll]
  (first (filter pred coll)))

(defn check-victory
  [board]
  (some empty? board))

(defn score-board
  [board last-number]
  (->> board
       (flatten)
       (set)
       (apply +)
       (* last-number)))

(defn play-number
  [boards number]
  (map (fn [board]
         (filter-number board number))
       boards))

(defn play-bingo
  [boards numbers]
  (reduce (fn [boards number]
            (let [new-boards (play-number boards number)]
              (if-let [winning-board (find-first check-victory new-boards)]
                (reduced (score-board winning-board number))
                new-boards)))
          boards
          numbers))

(defn solve-a
  []
  (let [numbers (map read-string (clojure.string/split input-numbers #","))
        boards (map board-string->board (clojure.string/split input-boards #"\n\n"))]
    (play-bingo boards numbers)))


(comment
  (solve-a)
  ; 67716
  )

(defn play-losing-bingo
  [boards numbers]
  (reduce (fn [boards number]
            (let [new-boards (play-number boards number)
                  losing-boards (remove check-victory new-boards)]
              (if (= (count losing-boards) 0)
                (reduced (score-board (first new-boards) number))
                losing-boards)))
          boards
          numbers))

(defn solve-b
  []
  (let [numbers (map read-string (clojure.string/split input-numbers #","))
        boards (map board-string->board (clojure.string/split input-boards #"\n\n"))]
    (play-losing-bingo boards numbers)))


(comment
  (solve-b)
  ; 1830
  )
