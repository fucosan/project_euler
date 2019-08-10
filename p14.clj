(ns euler.p14)

(defn is-even [n]
  (/ n 2))

(defn is-odd [n]
  (+ 1 (* 3 n)))

(defn collatzs [x] (iterate #(if (even? %) (is-even %) (is-odd %)) x))

(def jum-collatz
  (map-indexed (fn [b a] [a (count (take-while #(< 1 %) (collatzs a)))])
               (range 27 1000000)))


(time (+ 27 (first (reduce #(if (> (last %) (last %2)) % %2) jum-collatz))))

(time (last (sort-by last jum-collatz)))
