(ns looping-is-recursion)

(defn power [base exp]
  (loop [acc 1
         exp exp]
    (if (zero? exp)
      acc
      (recur (* acc base) (dec exp)))))

(defn last-element [a-seq]
  (cond
    (empty? a-seq) nil
    (= 1 (count a-seq)) (first a-seq)
    :else (recur (rest a-seq))))

(defn seq= [seq1 seq2]
    (cond
      (and (empty? seq1) (empty? seq2)) true
      (or (empty? seq1) (empty? seq2)) false
      (not= (first seq1) (first seq2)) false
      :else (recur (rest seq1) (rest seq2))))

(defn find-first-index [pred a-seq]
  (loop [i 0
         a-seq a-seq]
    (cond
      (empty? a-seq) nil
      (pred (first a-seq)) i
      :else (recur (inc i) (rest a-seq)))))

(defn avg [a-seq]
  (loop [sum 0
         count 0
         a-seq a-seq]
    (cond
      (and (empty? a-seq) (zero? count)) nil
      (empty? a-seq) (/ sum count)
      :else (recur (+ sum (first a-seq)) (inc count) (rest a-seq)))))

(defn parity [a-seq]
  (loop [counts '{}
         a-seq a-seq]
    (if (empty? a-seq)
      (keys (filter #(odd? (val %)) counts))
      (let [current (first a-seq)
            count (or (get counts current) 0)]
        (recur (assoc counts current (inc count))
               (rest a-seq))))))

(defn fast-fibo [n]
  (loop [i 3
         penultimate 1
         last 1]
    (let [current (+ penultimate last)]
      (cond
        (<= n 0) 0
        (<= n 2) 1
        (= i n) current
        :else (recur (inc i)
                     last
                     current)))))

(defn cut-at-repetition [a-seq]
  (loop [prevs []
         a-seq a-seq]
    (let [current (first a-seq)]
      (cond
        (empty? a-seq) prevs
        (contains? (set prevs) current) prevs
        :else (recur (conj prevs current) (rest a-seq))))))
