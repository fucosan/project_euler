(ns euler.p10
  (:require [euler.p3 :as p3]))
             
(apply + (take-while #(> 2000000 %) p3/primes))
