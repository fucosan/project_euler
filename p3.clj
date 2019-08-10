(ns euler.p3)

(defn prime?g
  [x]
  (when-not (some #(= 0 (mod x %))
                 (range 3 (inc (Math/sqrt x)) 2))) x nil)

(defn prime?
  [x]
  (let [root  (Math/sqrt x)
        mod-zero? (fn [res i] (if (zero? (mod x i)) (reduced false)
                                  true))]
    (and (> x 1)
         (if (= 2 x) true
             (reduce mod-zero? true (range 2 (inc root)))))))

(defn prime2? [x]
  (and (> x 1)
       (not (some #(if (= 0 (mod x %))
                     false)
                  (range 2 (inc (Math/sqrt x)))))))

(defn prime3? 
  "test whether number is prime. Complexity O(sqrt(p))"
  [p]
  (and (> p 1)
       (not (some #(zero? (mod p %))
                  (take-while #(<= (* % %) p)
                              (range 2 p))))))
  

(def primes (filter prime? (iterate inc 1)))

(defn factorize 
  "factorize number to prime multiplies"
  [n]
  
  (loop [x n fact []]
    (if (= 1 x) fact
        (let [d (first (drop-while #(not (zero? (rem x %))) (primes)))]
          (recur (/ x d) (conj fact d))))))

(defn fak1
  "factorize number to prime multiplies"
  [n]
  (loop [x n fact []]
    (if (= 1 x) fact
        (let [d (first (drop-while #(not (zero? (rem x %))) primes))]
          (recur (/ x d) (conj fact d))))))

(defn fak2
  [x]
  (if (= 1 x) []
      (let [d (first (drop-while #(not (zero? (rem x %))) primes))]
        (conj (faktorisasi (/ x d)) d))))


(defn fak3 [x]
  (take-while #(> (inc x) (* % 2)) (filter #(if (zero? (mod x %)) %) (iterate inc 1))))

(defn fak-and-primes [x]
  (filter prime? (filter #(if (zero? (mod x %)) %) (iterate inc 1))))

(defn fak4 [x]
  (distinct
   (mapcat #(vector % (/ x %)) 
           (filter #(zero? (mod x %)) (range 1 (inc (int (Math/sqrt x))))))))


(defn largest-prime-factor-of
  [x]
  (apply max (filter prime? (fak4 x))))


;;(time (prime? 600851475143))


(defn mg
  [f & ms]
  (let [data (->> (apply concat ms)
                  (group-by first))
        getval #(mapv last %)]

    (reduce (fn [a [k v]]
              (assoc  a k  (if (= 1 (count (getval v)))
                             (apply last v)
                             (apply f (getval v)))))
           {} data)))


(defn tictactoe
  [data]
  (let [n (count data) fdata (flatten data)
        sknario (concat
                 data
                 (apply map vector data)
                 [(map #(nth fdata %) (range 0 (* n n) (inc n)))]
                 [(map #(nth fdata %) (range (dec n) (- (* n n) (- n 1)) (dec n)))])]
    (->> sknario
         (filter (fn [a] (apply = a)))
         flatten
         (some #{:x :o}))))

(def b [[:x :x :e]
        [:e :x :e]
        [:o :x :o]])


(defn perfect-squares
  [coll]
  (let [sqrt #(int (Math/sqrt %))
        perfect? (fn [a] (= (* (sqrt a) (sqrt a)) a))]
    (->> coll
         (re-seq #"\d")
         (filter (fn [a] (perfect? (Integer. a))))
         (interpose ",")
         (apply str))))

(perfect-squares "1,2,4,5,6,7,8,9")

(defn totient
  [x]
  (->> (range x)
       (filter (fn [a] (some #(if (= 0 (mod a %) (mod x %)) a) (range 2 (inc a)))))
       count
       (- x 1)))          


(->> (group-by set ["meet" "emet" "eat" "tea" "wo"])
     vals
     (filter (fn [a] (> (count a) 1)))
     (map  (fn [a] (into #{} a)))
     (into #{}))


(defn mytrampoline
  [f & args]
  (loop [s (apply f args)]
    (if (fn? s)
      (recur (s))
      s)))

(defn b []
  (letfn [(triple [x] #(sub-two (* 3 x)))
          (sub-two [x] #(stop?(- x 2)))
          (stop? [x] (if (> x 50) x #(triple x)))]
    (mytrampoline triple 2)))




(defn f1 [n]
  (let [sisa (fn [a] (subvec (into [] a) 1 (dec (count a))))
        dobel (fn [a] (reduce #(conj % %2 %2) [] a))
        dobel-sisa (fn [a] (concat [(first a)] (dobel (sisa a)) [(last a)]))
        d (reverse  n)]
    (first (reduce (fn [a b]
                     (->> (map #(+ % %2) (dobel-sisa a) (dobel b))
                          (partition 2) 
                          (map #(apply min %))))
                   (first d) (rest d)))))

(defn f2
  [xxs]
  (letfn [(red [down up]
            (map + up (map min down (rest  down))))]
    (->> xxs reverse (reduce red))))

(defn perfect-number?
  [n]
  (= n (apply + (filter #(= 0 (mod n %)) (range 1 n)))))

(defn power-set
  [x]
  (loop [ [f & r] (seq x) h #{#{}}] 
    (if f
      (recur r (set (concat h (map #(set (conj % f))  h))))
      h)))


(defn happy-number2
  [x]
  (loop [i x]
    (cond
      (= i 1) true
      (get #{4 16 37 58 89 145 42 20} i) false
      :else (recur (->> i str
                        (map (comp #(* % %) read-string str))
                        (apply +))))))
(defn euler-circuit
  [g]
  (let [all-node (distinct (flatten g))
        node-degree (vals (merge-with concat (group-by first g) (group-by second g)))]
    (cond
      (= (count all-node) 2) (and (apply distinct? (flatten g))
                                  (= 1 (count g)))
      (some (comp odd? count) node-degree) false
      :else true)))

(defn connectivity-graph?
  [g]
  (let [all-node (distinct (flatten (seq g)))
        data (merge-with concat
                         (group-by first g) (group-by second g))]
    (loop [cn #{} [a & un] [(first all-node)]]
      (if-not a
        (if (= (count cn) (count all-node))
          true false)
        (recur (conj cn a) (concat un (clojure.set/difference
                                       (set (flatten (data a)))
                                       (conj cn a))))))))
(defn roman-numerals
  [x]
  (let [data {\I 1 \V 5 \X 10 \L 50 \C 100  \D 500 \M 1000}
        rx (reverse x)]
    (loop [[s & ss] (rest rx) t (data (first rx)) h 0]
      (if s
        (recur ss (if (> t (data s)) (* -1 (data s)) (data s))
               (+ h t))
        (+ h t)))))

(defn partial-flatten
  [[c & oll]]
  (if c
    (if (some coll? c)
      (partial-flatten (concat c oll))
      (conj (partial-flatten oll) c))
    '()))
  
(defn binary-tree?
  [a]
  (cond (coll? a)
        (and (true? a) (binary-tree? (nth a 1)) (binary-tree? (nth a 2)))
        (nil? a) true
        :else false))


(defn who-win?
  [board]
  (let [n (count board)
        i-board (apply map vector board)
        f-board (flatten board)
        diagonal (fn [a] (map #(nth f-board %) a))]
    (->> (concat board i-board [(diagonal (range 0 (* n n) (inc n)))]
                 [(diagonal (range n (- (* n n) (dec n)) (dec n)))])
         ((fn [a] (filter #(apply = %) a)))
         ((fn [a] (some #(when (#{:x :o} (first %)) (first %)) a))))))
