(ns euler.p6)

(defn power [x]
  (*' x x))

(defn sum-of-the-square
  [x]
  (apply +' (map #(* % %) (range 1 (inc x)))))

(defn square-of-the-sum1
  [x]
  (power (apply +' (range 1 (inc x)))))


(defn sum-square-deffrent
  [x]
  (- (square-of-the-sum1 x)
     (sum-of-the-square x)))

(square-of-the-sum1 10)
(sum-square-deffrent 100)
