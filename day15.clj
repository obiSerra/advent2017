(use '[clojure.test :only [is]])


(def a-factor 16807)
(def b-factor 48271)

(def rem-mod 2147483647)


(def a-starting-test 65)
(def b-starting-test 8921)

(def a-starting 703)
(def b-starting 516)

(defn calc-num [r factor]
  (mod (* r factor) rem-mod)
)

(defn gen-res-counter [starting factor]
  (defn res-counter 
    ([n] (res-counter n [starting]))
    
    ([n res]
     (if (= n 0)
       (rest res)
       (recur (dec n) (conj res (calc-num (last res) factor)))
       )))
)

(defn get-matches [i ii]
  (let [bi (Integer/toString i 2)
        bii (Integer/toString ii 2)]

    (if (= (take-last 16 bi)
           (take-last 16 bii))
      true
      false
      )
    )
  
  )

(def xf (comp
          (map #(get-matches %1 %2))
          (filter true?)
          ))

(defn solve-quiz [a-st a-fc b-st b-fc runs]
  
  (transduce
   xf 
   inc
   0
   [((gen-res-counter a-st a-fc) runs)
    ((gen-res-counter b-st b-fc) runs)])
  )

(defn simply-rec [runs]
  (if (= 0 runs)
    true
    (recur (dec runs))
    ) 
)

(defn all-in-red [acc val]

  (if (and (not= val 0) (= 0 (mod val 20000))) (println val " --- " (:valid acc)))
  
  (-> acc 
      (update-in [:vals] #(vec (map calc-num % (:facts acc))))
      (update-in [:valid] #(if (get-matches (first (:vals acc)) (last (:vals acc))) (inc %) %))
      ) 
  
  )

(time (:valid (reduce all-in-red {:vals [a-starting b-starting]
                                  :facts [a-factor b-factor]
                                  :valid 0
                                  } (range 40000000))))

;; (time ((gen-res-counter b-starting-test b-starting-test) 40000000))


;; (time (simply-rec 40000000))

;; (time (dorun (map #(identity %) (range 40000000))))

;; (time (solve-quiz a-starting-test a-factor b-starting-test b-factor 40000000))
