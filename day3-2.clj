(use '[clojure.test :only [is]])

(defn apply-dir [pnt dir]
  (cond (= dir 1) {:x (inc (:x pnt)) :y (:y pnt)}
        (= dir 2) {:y (inc (:y pnt)) :x (:x pnt)}
        (= dir 3) {:x (dec (:x pnt)) :y (:y pnt)}
        (= dir 4) {:y (dec (:y pnt)) :x (:x pnt)}
        )
)

(defn turn? [acc]
  (let [[v cur cnt] acc] (= cur v))
  )

(defn next-acc [acc]
  "[v cur cnt] --> v -> numero di step da raggiungere, cur -> step attuale, cnt --> prima o seconda volta che viene raggiunto"
  (let [[v cur cnt] acc] 
    (cond (and (= cur v) (= 1 cnt)) [(inc v) 0 0]
          (= cur v) [v 0 1]
          :else [v (inc cur) cnt])
    )
)

(defn next-dir [acc dir]
  (println acc (turn? acc) dir)
  (cond (and (turn? acc) (= dir 1)) 2
        (and (turn? acc) (= dir 2)) 3
        (and (turn? acc) (= dir 3)) 4
        (and (turn? acc) (= dir 4)) 1
        :else dir
)
  )

(defn gen-seq 
  ([max]
   (gen-seq max [{:x 0 :y 0 :v 1}] 1 [1 0 0])   
   )
  ([max spr dir acc]
    "A ogni giro appende un nuovo elemnto con :x e :y modificati in base alla direzione"
    (if (> (count spr) max)
      spr
      (recur max (conj spr (merge {:v (inc (:v (last spr)))} (apply-dir (last spr) dir))) (next-dir acc dir) (next-acc acc))
      )
    )
)

(gen-seq 5)


;(is (= 54 (sp-sq 55)))


;(is (= 3 (sum-near "1122")))





