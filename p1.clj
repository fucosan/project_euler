(ns euler.p1)

(defn multiple-3-5?
  [x]
  (if (or (= 0 (mod x 3))
          (= 0 (mod x 5)))
    true))

(def l (filter #(multiple-3-5? %) (range)))

(apply + (take-while #(>  1000 %) l))

(defn sol2 [x]
  (apply + (for [i (range 3 x)
                 :when (or (= 0 (mod i 3))
                           (= 0 (mod i 5)))]
             i)))
