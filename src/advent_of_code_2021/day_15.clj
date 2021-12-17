(ns advent-of-code-2021.day-15
  (:require [ysera.collections :refer [seq-contains? index-of]]
            [ysera.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code_2021/inputs/day15.txt"))
(def test-input "1163751742\n1381373672\n2136511328\n3694931569\n7463417111\n1319128137\n1359912421\n3125421639\n1293138521\n2311944581")

(defn create-risk-level-state
  [input]
  (let [lines (clojure.string/split-lines input)]
    (reduce (fn [state i]
              (let [line (nth lines i)]
                (reduce (fn [state j]
                          (assoc state [i j] (read-string (str (nth line j)))))
                        state
                        (range (count line)))))
            {}
            (range (count lines)))))

(defn create-big-risk-level-state
  {:test (fn []
           (is= (create-big-risk-level-state "8")
                (create-risk-level-state "89123\n91234\n12345\n23456\n34567")))}
  [input]
  (let [lines (clojure.string/split-lines input)
        num-rows (count lines)
        num-cols (count (first lines))
        small-state (create-risk-level-state input)]
    (reduce (fn [state i]
              (reduce (fn [state j]
                        (let [quot-i (quot i num-rows)
                              rem-i (rem i num-rows)
                              quot-j (quot j num-cols)
                              rem-j (rem j num-cols)
                              small-state-risk-level (get small-state [rem-i rem-j])
                              risk-level-uncapped (+ small-state-risk-level quot-i quot-j)
                              risk-level (if (> risk-level-uncapped 9)
                                           (- risk-level-uncapped 9)
                                           risk-level-uncapped)]
                          (assoc state [i j] risk-level)))
                      state
                      (range (* 5 num-cols))))
            {}
            (range (* 5 num-rows)))))

(defn get-big-end-pos
  {:test (fn []
           (is= (get-big-end-pos test-input) [49 49]))}
  [input]
  [(dec (* 5 (count (clojure.string/split-lines input)))) (dec (* 5 (count (first (clojure.string/split-lines input)))))])

(defn get-neighbours
  [risk-level-state [i j]]
  (filter (fn [c]
            (contains? risk-level-state c))
          [[(inc i) j] [(dec i) j] [i (inc j)] [i (dec j)]]))

(defn get-path-risk-level
  [risk-level-state path]
  (reduce (fn [risk c]
            (+ risk (get risk-level-state c)))
          0
          path))

(defn path-finished?
  [path end-pos]
  (let [c (first path)]
    (= c end-pos)))

(defn get-new-paths
  [risk-level-state paths end-pos]
  (reduce (fn [new-paths path]
            (let [c (first path)
                  neighbours (get-neighbours risk-level-state c)]
              (reduce (fn [new-paths c]
                        (if (or (path-finished? path end-pos)
                                (seq-contains? path c))
                          new-paths
                          (conj new-paths (conj path c))))
                      new-paths
                      neighbours)))
          #{}
          paths))

(defn get-path-up-to-c
  "Up to and including c"
  [path c]
  (drop (index-of path c) path))

(defn get-path-risk-level-up-to-c
  "Up to and including c"
  [risk-level-state path c]
  (let [path-up-to-c (get-path-up-to-c path c)]
    (get-path-risk-level risk-level-state path-up-to-c)))

(defn has-path-been-beaten?
  [risk-level-state path optimal-risk-levels]
  (reduce (fn [_ c]
            (let [path-score (get-path-risk-level-up-to-c risk-level-state path c)
                  [optimal-score optimal-path] (get optimal-risk-levels c)]
              (if (and (>= path-score optimal-score)
                       (not= (get-path-up-to-c path c) optimal-path))
                (reduced true)
                false)))
          false
          path))

(defn filter-paths
  {:test (fn []
           (is= (filter-paths (create-risk-level-state test-input) #{(list [1 0] [0 0]) (list [0 1] [0 0])} {[0 0] [1 (list [0 0])] [1 0] [2 (list [1 0] [0 0])] [0 1] [2 (list [0 1] [0 0])]})
                #{(list [1 0] [0 0]) (list [0 1] [0 0])}))}
  [risk-level-state paths optimal-risk-levels-so-far]
  (reduce (fn [filtered-paths path]
            (if (has-path-been-beaten? risk-level-state path optimal-risk-levels-so-far)
              filtered-paths
              (conj filtered-paths path)))
          #{}
          paths))

(defn get-new-optimal-risk-levels
  [risk-level-state paths optimal-risk-levels-so-far]
  (reduce (fn [optimal-risk-levels path]
            (let [c (first path)
                  path-risk-level (get-path-risk-level risk-level-state path)]
              (if (and (contains? optimal-risk-levels c)
                       (>= path-risk-level (first (get optimal-risk-levels c))))
                optimal-risk-levels
                (assoc optimal-risk-levels c [path-risk-level path]))))
          optimal-risk-levels-so-far
          paths))


(defn get-safest-route-risk-level
  {:test (fn []
           (is= (get-safest-route-risk-level test-input false) 40)
           (is= (get-safest-route-risk-level test-input true) 315))}
  [input use-big-state]
  (let [risk-level-state (if use-big-state (create-big-risk-level-state input) (create-risk-level-state input))
        start-pos [0 0]
        start-pos-risk-level (get risk-level-state start-pos)
        end-pos (if use-big-state (get-big-end-pos input) [(dec (count (clojure.string/split-lines input))) (dec (count (first (clojure.string/split-lines input))))])]
    (loop [paths #{(list start-pos)}
           optimal-risk-levels-so-far {start-pos [start-pos-risk-level (list start-pos)]}
           i 1]
      (println i (count paths))
      (if-not (some (fn [path]
                      (not (path-finished? path end-pos)))
                    paths)
        ;; start-pos does not count towards total risk level, but is included in path
        (- (get-path-risk-level risk-level-state (first paths)) start-pos-risk-level)
        (let [new-paths (get-new-paths risk-level-state paths end-pos)
              new-optimal-risk-levels (get-new-optimal-risk-levels risk-level-state new-paths optimal-risk-levels-so-far)]
          (recur (filter-paths risk-level-state new-paths new-optimal-risk-levels) new-optimal-risk-levels (inc i)))))))

(defn solve-a
  []
  (get-safest-route-risk-level input false))

(comment
  (time (solve-a))
  ; Elapsed time: 3993.875522 msecs" (old way)
  ; Elapsed time: ~40000 msecs" (new way)
  ; 755
  )

(defn solve-b
  []
  (get-safest-route-risk-level input true))

(comment
  (time (solve-b))
  ; too slow... even when pruning all beaten paths
  ;
  )
