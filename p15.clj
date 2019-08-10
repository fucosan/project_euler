(ns euler.p15)

(defn faktorial
  [x]
  (if (= x 0)
    1
    (*' x (faktorial (dec x)))))


(defn combination [n r]
  (/ (faktorial n) (*' (faktorial r) (faktorial (- n r)))))


(defn sol1 []
  (combination 40 20))

(defn row
  "return nth row of pascal triangle"
  [n]
  (map #(combination n %) (range (inc n))))

(defn triangle-pascal
  "return flatten n lazy-seq triangle pascal"
  [n]
  (take n (for [a (range)
                b (range (inc a))
                :let [c (combination a b)]]
            c)))

(def triangle-pascal2 (map row (range)))


(defn sol2
  "return solution of n * n lattice path"
  [n]
  (nth (last (take (+ 1 (* 2 n)) triangle-pascal2)) n))

(sol2 20)
