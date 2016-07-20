(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc base exp]
                 (if (= exp 0)
                   acc
                   (recur (* acc base) base (dec exp))))]
    (helper 1 base exp)))

(defn last-element [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (recur (rest a-seq))))

(defn seq= [seq1 seq2]
  (let [helper (fn [seq1 seq2]
                 (cond
                   (empty? seq1) true
                   (not (= (first seq1) (first seq2))) false
                   :else (recur (rest seq1) (rest seq2))))]
    (if (= (count seq1) (count seq2))
      (helper seq1 seq2)
      false)))


(defn find-first-index [pred a-seq]
  (loop [acc 0
         pred pred
         a-seq a-seq]
    (cond
      (empty? a-seq) nil
      (pred (first a-seq)) acc
      :else (recur (inc acc) pred (rest a-seq)))))

(defn avg [a-seq]
  (loop [count 0
         sum 0
         a-seq a-seq]
    (if (empty? a-seq)
      (/ sum count)
      (recur (inc count) (+ sum (first a-seq)) (rest a-seq)))))

(defn toggle [a-set a-val]
  (if (contains? a-set a-val)
    (disj a-set a-val)
    (conj a-set a-val)))

(defn parity [a-seq]
  (loop [a-set #{}
         a-seq a-seq]
    (if (empty? a-seq)
      a-set
      (recur (toggle a-set (first a-seq)) (rest a-seq)))))

(defn fast-fibo [n]
  (if (<= n 1)
    n
    (loop [fn-2 0
           fn-1 1
           n (- n 2)]
      (if (= n 0)
        (+ fn-2 fn-1)
        (recur fn-1 (+ fn-2 fn-1) (dec n))))))

(defn cut-at-repetition [a-seq]
  (loop [out []
         a-set #{}
         a-seq a-seq]
    (cond
      (empty? a-seq) out
      (contains? a-set (first a-seq)) out
      :else (recur (conj out (first a-seq)) (conj a-set (first a-seq)) (rest a-seq)))))

