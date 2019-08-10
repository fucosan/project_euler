(ns euler.p9.clj)

;; a 1 b 2 c 997
;; a 332 b 333 c 335
;; a 1 b 499 c 500
;; a 1 b 2 c

(for [a (range 1 333)
      b (range 2 500)
      c (range 3 998)
      :when (> c b)
      :when (> b a)
      :when (= (+' a b c) 1000)
      :when (= (*' c c) (+' (*' a a) (*' b b)))]
  (*' a b c))

(for [a (range 1 333)
      b (range a (- 1000 a))
      :let [c (- 1000 a b)]
      :when (> c b) 
      :when (= (*' c c) (+' (*' a a) (*' b b)))]
  (*' a b c))
