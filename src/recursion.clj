(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))
  )
)

(defn singleton? [coll]
  (and
    (not (empty? coll))
    (empty? (rest coll))
  )
)

(defn my-last [coll]
  (cond
    (empty? coll) nil
    (singleton? coll) (first coll)
    :else (my-last (rest coll))
  )
)

(defn max-element [a-seq]
  (cond
    (empty? a-seq) nil
    (singleton? a-seq) (first a-seq)
    :else (max (first a-seq) (max-element (rest a-seq))) 
  )
)

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2
  )
)

(defn longest-sequence [a-seq]
  (cond
    (empty? a-seq) nil
    (singleton? a-seq) (first a-seq)
    :else (seq-max (first a-seq) (longest-sequence (rest a-seq)))
  )
)

(defn my-filter [pred? a-seq]
  (cond
    (empty? a-seq) a-seq
    (pred? (first a-seq)) (cons (first a-seq) (my-filter pred? (rest a-seq)))
    :else (my-filter pred? (rest a-seq))
  )
)

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq) false
    (== elem (first a-seq)) true
    :else (sequence-contains? elem (rest a-seq))
  )
)

(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq) '()
    (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
    :else '()
  )
)

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq) '()
    (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
    :else a-seq
  )
)

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (or (empty? a-seq) (empty? b-seq)) false
    (== (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
    :else false
  )
)

(defn my-map [f seq-1 seq-2]
  (cond
    (or (empty? seq-1) (empty? seq-2)) '()
    :else (cons
      (f (first seq-1) (first seq-2))
      (my-map f (rest seq-1) (rest seq-2))
    )
  )
)

(defn power [n k]
  (if (== k 0)
    1
    (* n (power n (dec k)))
  )
)

(defn fib [n]
  (cond
    (== n 0) 0
    (== n 1) 1
    :else (+ (fib (- n 1)) (fib (- n 2)))
  )
)

(defn my-repeat [how-many-times what-to-repeat]
  (if (< how-many-times 1)
    '()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))
  )
)

(defn my-range [up-to]
  (if (< up-to 1)
    '()
    (cons (dec up-to) (my-range (dec up-to)))
  )
)

(defn tails [a-seq]
  (if (empty? a-seq)
    '(())
    (cons (seq a-seq) (seq (tails (rest a-seq))))
  )
)

(defn inits [a-seq]
  (let [reversed-seq (reverse a-seq)
        init-seq (tails (seq reversed-seq))
       ]
    (reverse (map reverse init-seq))
  )
)

(defn rotate [a-seq n]
  (cond
    (== n 0) '(())
    (== n 1) (vector a-seq)
    :else (cons (seq a-seq) (seq (rotate (concat (rest a-seq) (vector (first a-seq))) (dec n))))
  )
)

(defn rotations [a-seq]
  (rotate a-seq (count a-seq))
)

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let[val (first a-seq)
         prev-count (get freqs val)
         new-count (if (nil? prev-count) 1 (inc prev-count))
         new-freqs (assoc freqs val new-count)
        ]
      (my-frequencies-helper new-freqs (rest a-seq))
    )
  )
)

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq)  
)

(defn un-frequencies-helper [a-map result]
  (if (empty? a-map)
    result
    (let[[val cnt] (first a-map)
         addition (repeat cnt val)
         new-res (concat result addition)
        ]
      (un-frequencies-helper (rest a-map) new-res)
    )
  )
)

(defn un-frequencies [a-map]
  (un-frequencies-helper a-map [])
)

(defn my-take [n coll]
  (if (or (== n 0) (empty? coll))
    '()
    (cons (first coll) (my-take (dec n) (rest coll)))
  )
)

(defn my-drop [n coll]
  (if (or (== n 0) (empty? coll))
    coll
    (my-drop (dec n) (rest coll))
  )
)

(defn halve [a-seq]
  (if (empty? a-seq)
    '(()())
    (let [half (quot (count a-seq) 2)
          front-half (my-take half a-seq)
          end-half (my-drop half a-seq)
         ]
      (concat [front-half] [end-half])
    )
  )
)

(defn seq-merge [a-seq b-seq]
  (let [a-item (first a-seq)
        b-item (first b-seq)
       ]
    (cond
      (and (nil? a-item) (nil? b-item)) '()
      (nil? a-item) b-seq
      (nil? b-item) a-seq
      (< a-item b-item) (concat [a-item] (seq-merge b-seq (rest a-seq)))
      :else (concat [b-item] (seq-merge a-seq (rest b-seq)))
    )
  )
)

(defn merge-sort [a-seq]
  (if (or (empty? a-seq) (singleton? a-seq))
    a-seq
    (let [[begin end] (halve a-seq)
         ]
      (seq-merge (merge-sort begin) (merge-sort end))
    )
  ) 
)

(defn monotonic? [a-seq]
  (or
    (apply <= a-seq)
    (apply >= a-seq)
  )
)

(defn split-into-monotonics [a-seq]
  (let [_is-monotonic? (fn [x] (and (not (empty? x)) (monotonic? x)))
        _inits (drop 1 (inits a-seq))
        _monotonics (take-while _is-monotonic? _inits)
        _last (last _monotonics)
       ]
    (if (nil? _last)
      nil
      (concat 
        [_last] 
        (split-into-monotonics (drop (count _last) a-seq))
      )
    )
  )
)

(defn permutations [a-set]
)

(defn powerset [a-set]
  [:-])

