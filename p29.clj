(ns euler.p29)

(count (into #{} (for [a (range 2 101)
                       b (range 2 101)]
                   (Math/pow a b))))

(defn list-to-number
  [coll]
  (Integer. (apply str coll)))

(defn number-to-list
  [coll]
  (map #(Integer. (str %)) (str coll)))

(defn power-sum
  [coll pangkat]
  (int (apply + (map #(Math/pow % pangkat) coll))))



(apply +(for [a (range 10000 100000)
              :when (= (power-sum (number-to-list a) 5)
                       a)]
          a))

(power-sum (number-to-list 54748) 5)
(count (range 10000 100000))

