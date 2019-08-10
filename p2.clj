(ns euler.p2)

(defn fibo
  [x y]
  (lazy-seq (cons (+' x y) (fibo y (+' x y)))))

(apply +
       (filter even?
               (take-while
                #(> 4000001 %) (fibo 0 1))))

(def fibo2 (map first (iterate
                       (fn [[a b]] [b (+' a b)])
                       [1 1])))

(time (take 4000000 fibo2))
