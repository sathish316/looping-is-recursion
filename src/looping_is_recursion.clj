(ns looping-is-recursion)

(defn recursive-factorial [n]
  (if (zero? n)
    1
    (* n (recursive-factorial (dec n)))))

(recursive-factorial 5)

(defn factorial-helper [acc n]
  (if (zero? n)
    acc
    (factorial-helper (* acc n) (dec n))))

(defn acc-factorial [n]
  (factorial-helper 1 n))

(acc-factorial 5)

(defn recur-factorial [n]
  (let [helper (fn [acc n]
                 (if (zero? n)
                   acc
                   (recur (* acc n) (dec n))))]
    (helper 1 n)))

(recur-factorial 5)

(defn power [base exp]
  (let [helper (fn [result n k]
                  (if (zero? k)
                    result
                    (recur (* result n) n (dec k))))]
    (helper 1 base exp)))

(defn last-element [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (last-element (rest a-seq))))

(defn seq= [seq1 seq2]
  (cond
   (empty? seq1) (empty? seq2)
   (empty? seq2) (empty? seq1)
   (= (first seq1) (first seq2)) (seq= (rest seq1) (rest seq2))
   :else false))

(defn loopy-factorial [n]
  (loop [acc 1 n n]
    (if (zero? n)
      acc
      (recur (* acc n) (dec n)))))

(loopy-factorial 5)

(defn find-first-index [pred a-seq]
  (loop [index 0
         seq a-seq]
    (cond
     (empty? seq) nil
     (pred (first seq)) index
     :else (recur (inc index)
                  (rest seq)))))

(defn avg [a-seq]
  (loop [sum 0
         n 0
         seq a-seq]
    (if (empty? seq)
      (/ sum n)
      (recur (+ sum (first seq))
             (inc n)
             (rest seq)))))

(defn toggle [a-set elem]
  (cond
   (contains? a-set elem) (disj a-set elem)
   :else (conj a-set elem)))

(defn parity [a-seq]
  (if (empty? a-seq)
    #{}
    (toggle (parity (rest a-seq))
            (first a-seq))))

(defn fast-fibo [n]
  (loop [num_2 0
         num_1 1
         n n]
    (cond
     (= n 0) num_2
     (= n 1) num_1
     :else (recur num_1
                  (+ num_2 num_1)
                  (dec n)))))

(defn cut-at-repetition [a-seq]
  (loop [so-far #{}
         result []
         a-seq a-seq]
    (let [elem (first a-seq)]
      (if (or (empty? a-seq)
              (so-far elem))
        result
        (recur (conj so-far elem)
               (conj result elem)
               (rest a-seq))))))
