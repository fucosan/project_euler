(ns euler.p7
  (:require [euler.p3 :as p3]))


(defn idx-prime-of [x]
  (last (take x p3/primes)))

(idx-prime-of 10001)
