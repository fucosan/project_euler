(ns euler.p5
  (:require [euler.p3 :as p3]))
            

;; find number can divided by 1-10

(defn gcd [a b]
  (apply max (clojure.set/intersection (into #{} (p3/fak4 a)) (into #{} (p3/fak4 b)))))

(defn lcm [a b]
  (/ (Math/abs (*' a b)) (gcd a b)))


(defn smallsest-multiple [x]
  (reduce lcm  (range 1 x)))

