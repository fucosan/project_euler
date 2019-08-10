(ns euler.p22
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]))

(def raw (slurp "https://projecteuler.net/project/resources/p022_names.txt"))

(def data (rest (clojure.string/split raw #"[^A-Za-z0-9]+")))

(def input (sort data))




(def alpha-value {\A 1 \B 2 \C 3 \D 4 \E 5 \F 6 \G 7 \H 8 \I 9 \J 10 \K 11 \L 12 \M 13 \N 14 \O 15 \P 16 \Q 17 \R 18 \S 19 \T 20 \U 21 \V 22 \W 23 \X 24 \Y 25 \Z 26})


(defn jum-alpha
  [x]
  (reduce + (map #(alpha-value %)))) 


(apply + (map #(* (inc %) (jum-alpha (nth input %))) (range (count input))))
