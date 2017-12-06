(use '[clojure.test :only [is]])

(defn get-nth [el]
  (->  el
       first
       name
       read-string)
  )

(defn into-sortable-map [lst]
  (let [xf (comp (map str) (map keyword) (take (count lst)))]
    (map #(conj [] %1 %2 ) (transduce xf conj (range)) lst)))

(defn red-max [acc o]
  (if (> (last o) (last acc))
    o
    acc
    )
  )

(defn max-el [lst]
  (->> lst
      into-sortable-map
      (reduce red-max)
      )
  )



(defn next-idx [l idx]
  (let [n (+ 1 idx)]
    (if (>= n l)
      0
      n
      )
    )
)

(defn acc-loop [lst v n]
  (if (= 0 v)
    lst
    (recur (update-in lst [n] inc) (dec v) (next-idx (count lst) n))
    )
  )

(defn rec-acc
  ([lst] (rec-acc lst []))

  ([lst acc]
   (let [m (max-el lst)
         new-st (update-in lst [(get-nth m)] (fn [a] 0))
         new-id (next-idx (count new-st) (get-nth m))
         next-e (acc-loop new-st (last m) new-id)]
     (if-not (nil? (get (set acc) next-e))
       (- (count (conj acc lst)) (.indexOf acc next-e))
       (recur next-e (conj acc lst))
       )
     ))
  )

(def te [14 0 15 12 11 11 3 5 1 6 8 4 9 1 8 4])

(rec-acc te)
