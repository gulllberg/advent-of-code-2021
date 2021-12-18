(ns advent-of-code-2021.day-16
  (:require [ysera.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code_2021/inputs/day16.txt"))

(defn convert-char
  [hex-char]
  (condp = hex-char
    \0 "0000"
    \1 "0001"
    \2 "0010"
    \3 "0011"
    \4 "0100"
    \5 "0101"
    \6 "0110"
    \7 "0111"
    \8 "1000"
    \9 "1001"
    \A "1010"
    \B "1011"
    \C "1100"
    \D "1101"
    \E "1110"
    \F "1111"
    ""))

(defn hex->binary
  {:test (fn []
           (is= (hex->binary "D2FE28") "110100101111111000101000"))}
  [hex]
  (reduce (fn [binary-str hex-char]
            (str binary-str (convert-char hex-char)))
          ""
          hex))

;; https://stackoverflow.com/questions/5057047/how-to-do-exponentiation-in-clojure
(defn exp [x n]
  (reduce * (repeat n x)))

(defn binary-string->number
  {:test (fn []
           (is= (binary-string->number "10110") 22)
           (is= (binary-string->number "01001") 9))}
  [binary-string]
  (reduce-kv (fn [a i v]
               (+ a (* (read-string (str v)) (exp 2 i))))
             0
             (vec (reverse binary-string))))

(defn get-type
  [binary-string]
  (condp = (subs binary-string 0 3)
    "100" :literal
    :operator))

(defn parse-structure
  [binary-string]
  (loop [version-numbers 0
         remaining-string binary-string
         doing :version
         ;; depth is unused for now
         depth 0]
    (if (and (= doing :version)
             (< (count remaining-string) 11))
      version-numbers
      (condp = doing
        :version (recur (+ version-numbers (binary-string->number (subs remaining-string 0 3)))
                        (subs remaining-string 3)
                        :type
                        depth)

        :type (recur version-numbers
                     (subs remaining-string 3)
                     (get-type (subs remaining-string 0 3))
                     (inc depth))

        :literal (if (= (first remaining-string) \0)
                   (recur version-numbers
                          (subs remaining-string 5)
                          :version
                          (dec depth))
                   (recur version-numbers
                          (subs remaining-string 5)
                          :literal
                          depth))

        :operator (if (= (first remaining-string) \0)
                    (recur version-numbers
                           (subs remaining-string 1)
                           :operator-length
                           depth)
                    (recur version-numbers
                           (subs remaining-string 1)
                           :operator-sub-packets
                           depth))

        ;; For now just ignore the structure of sub packets - assume that it's possible to tell when they terminate just by looking at the packets themselves.
        :operator-length (recur version-numbers
                                (subs remaining-string 15)
                                :version
                                (inc depth))

        :operator-sub-packets (recur version-numbers
                                     (subs remaining-string 11)
                                     :version
                                     (inc depth))))))

(defn solve-a
  []
  (parse-structure (hex->binary input)))

(comment
  (solve-a)
  ; 989
  )
