(ns euler.p4)

(defn palindrom
  [x]
  (let [seq-of (seq (str x))]
    (= seq-of (reverse seq-of))))

(defn largest-palindrom []
  (for [i (range 100 1000)
        j (range 100 1000)
        :when (palindrom (* i j))]
    (* i j)))
