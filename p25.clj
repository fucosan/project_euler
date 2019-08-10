(ns euler.p25
  (:require [euler.p2 :as p2]))


(def data (take-while  #(> 1001 (count (str %))) p2/fibo2))

(.indexOf data
          (first (drop-while  #(> 1000 (count (str %))) p2/fibo2)))








                      

