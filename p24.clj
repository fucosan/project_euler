(ns euler.p24)

;;pointer
;; stop iterasi

(defn swap [v i1 i2] 
  (assoc v i2 (v i1) i1 (v i2)))

(defn tails
  [coll]
  (concat
   (take-while seq (iterate rest coll))
   ['()]))

(defn inits
  [coll]
  (reverse
   (concat 
    (take-while seq (iterate drop-last coll))
    ['()])))


(defn rotations
  "take coll and return all posible permutation outcom for n=r"
  [coll]
  (rest (map concat (tails coll) (inits coll))))

(defn permutations [a-set]
  (if (empty? a-set)
    (list ())
    (mapcat
     (fn [[x & xs]] (map #(cons x %) (permutations xs)))
     (rotations a-set))))


