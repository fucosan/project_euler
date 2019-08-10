(ns euler.p12
  (:require [euler.p3 :as p3]))

(defn triangle-num
  [x]
  (/ (* x (+ x 1)) 2))

(def triangles (map triangle-num (iterate inc 1000)))

(defn jum-fak [x]
  (* 2 (count (filter #(zero? (mod x %)) (range 1 (Math/sqrt x))))))

(defn jum-fak2 [x]
  (reduce #(if (zero? (mod x %2)) (+ %1 2) %1) 0 (range 1 (Math/sqrt x))))

(defn jum-fak-triangular-number [x]
  (first (drop-while #(> (inc x) (jum-fak2 %)) triangles)))

(time (jum-fak-triangular-number 500))
(time (map jum-fak (range 1 10000)))
(time (map jum-fak2 (range 1 10000)))
