(ns euler.p20
  (:require [euler.p15 :as p15]))

(reduce + (map #(Integer. (str %)) (str (p15/faktorial 100))))

