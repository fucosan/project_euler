(ns euler.p23
  (:require [euler.p3 :as p3]))

(defn abundant?
  [n]
  (let [a (- (reduce + (p3/fak4 n)) n)]
    q    (if (> a n) true false)))

(def abundants (filter abundant? (range 1 28123)))

(def sum-of-two-abundants
  (zipmap (for [a abundants
                b abundants
                :let [c (+ a b)]
                :when (> 28124 c)]
            (+ a b))
         (repeat 0)))


(defn sol
  []
  (reduce + (get
             (group-by sum-of-two-abundants (range 1 28124))
             nil)))
