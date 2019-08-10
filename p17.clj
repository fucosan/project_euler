(ns euler.p17)
                                                                                                               
(def letters-of-nums
  {1 "one" 2 "two" 3 "three" 4 "four" 5 "five" 6 "six" 7 "seven" 8 "eight" 9 "nine" 10 "ten"
   11 "eleven" 12 "twelve" 13 "thirteen" 14 "fourteen" 15 "fifteen" 16 "sixteen" 17 "seventeen"
   18 "eighteen" 19 "nineteen" 20 "twenty" 30 "thirty" 40 "forty" 50 "fifty" 60 "sixty" 70 "seventy"
   80 "eighty" 90 "ninety" 100 "hundred" 1000 "onethousand"})

(defn satuan?
  [n]
  (if (and (> 11 n) (> n 0)) true false))

(defn belasan?
  [n]
  (if (and (> 20 n) (> n 10)) true false))

(defn puluhan?
  [n]
  (if (and (> n 19) (> 100 n)) true false))

(defn ratusan?
  [n]
  (if (and (> n 99) (> 1000 n)) true false))

(defn ribuan?
  [n]
  (if (and (> n 999) (> 10000 n)) true false))


(defn pecah-angka
  "return sequence of punyusun angka"
  [n]
  (cond
    (puluhan? n) (conj (pecah-angka (rem n 10)) (letters-of-nums (- n (rem n 10))))
    (ratusan? n) (conj (pecah-angka (rem n 100)) (letters-of-nums 100)
                       (letters-of-nums (Integer. (str (first (str (- n (rem n 100)))))))
                       "and")
    :else (conj [] (letters-of-nums n))))
    

(defn sol []
  (+ -27 (count (apply str (map #(apply str (pecah-angka %)) (range 1 1001))))))


