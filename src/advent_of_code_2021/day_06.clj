(ns advent-of-code-2021.day-06)

(def input "1,1,1,1,2,1,1,4,1,4,3,1,1,1,1,1,1,1,1,4,1,3,1,1,1,5,1,3,1,4,1,2,1,1,5,1,1,1,1,1,1,1,1,1,1,3,4,1,5,1,1,1,1,1,1,1,1,1,3,1,4,1,1,1,1,3,5,1,1,2,1,1,1,1,4,4,1,1,1,4,1,1,4,2,4,4,5,1,1,1,1,2,3,1,1,4,1,5,1,1,1,3,1,1,1,1,5,5,1,2,2,2,2,1,1,2,1,1,1,1,1,3,1,1,1,2,3,1,5,1,1,1,2,2,1,1,1,1,1,3,2,1,1,1,4,3,1,1,4,1,5,4,1,4,1,1,1,1,1,1,1,1,1,1,2,2,4,5,1,1,1,1,5,4,1,3,1,1,1,1,4,3,3,3,1,2,3,1,1,1,1,1,1,1,1,2,1,1,1,5,1,3,1,4,3,1,3,1,5,1,1,1,1,3,1,5,1,2,4,1,1,4,1,4,4,2,1,2,1,3,3,1,4,4,1,1,3,4,1,1,1,2,5,2,5,1,1,1,4,1,1,1,1,1,1,3,1,5,1,2,1,1,1,1,1,4,4,1,1,1,5,1,1,5,1,2,1,5,1,1,1,1,1,1,1,1,1,1,1,1,3,2,4,1,1,2,1,1,3,2")

(defn one-day
  [fish-timers]
  (reduce (fn [new-timers timer]
            (if (= timer 0)
              (conj new-timers 6 8)
              (conj new-timers (dec timer))))
          []
          fish-timers))

(defn n-days
  [fish-timers n]
  (reduce (fn [timers _]
            (one-day timers))
          fish-timers
          (range n)))

(defn solve-a
  []
  (let [starting-timers (map read-string (clojure.string/split input #","))]
    (count (n-days starting-timers 80))))

(comment
  (solve-a)
  ; 386536
  )

(defn one-day-dp
  [next-day]
  ;; Shifts everything one step and replaces first fish with sum of two "new" fish.
  (conj (drop-last 1 next-day) (+ (nth next-day 6) (nth next-day 8))))

(defn n-days-dp
  [n]
  (reduce (fn [total-created-fish _]
            (one-day-dp total-created-fish))
          [1 1 1 1 1 1 1 1 1]
          (range n)))

(defn solve-b
  []
  (let [starting-timers (map read-string (clojure.string/split input #","))
        dp-list (n-days-dp 256)]
    (reduce (fn [sum timer]
              (+ sum (nth dp-list timer)))
            0
            starting-timers)))

(comment
  (count (n-days [1] 256))
  (solve-b)
  ; 1732821262171
  )
