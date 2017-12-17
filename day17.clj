(use '[clojure.test :only [is]])


(defn cycle-list [ar start steps]
  (mod (+ start steps) (count ar)))

(defn cycle-list-2 [l start steps]
  (mod (+ start steps) l))

(defn insert-val [ar val pos]
  (concat (subvec (vec ar) 0 pos) [val] (subvec (vec ar) pos)))



(is (= 0 (cycle-list [0] 0 3)))
(is (= 1 (cycle-list ["a" "b"] 0 1)))
(is (= 0 (cycle-list ["a" "b"] 1 1)))
(is (= 2 (cycle-list ["a" "b" "c" "d" "e"] 2 5)))

(is (= 4 (cycle-list ["a" "b" "c" "d" "e" "f"] 1 3)))


(defn gen-memory 
  [max-val curr-val steps ar pos]

  (let [new-pos (if (> (inc pos) (count ar)) 0 (inc pos))
        new-ar (insert-val ar curr-val new-pos)] 
    (if (> (inc curr-val) max-val)
      (get (vec ar) (inc pos))
      (recur  max-val (inc curr-val) steps new-ar (cycle-list new-ar new-pos steps))    
      ))
  
  )



(defn quiz-2 [max-val curr-val steps l z-pos pos]
  (let [new-pos (if (> (inc pos) l) 0 (inc pos))
        new-l (inc l)
        new-z-pos (if (= pos 0) curr-val z-pos)] 

    (if (> (inc curr-val) max-val)
      new-z-pos
      (recur  max-val (inc curr-val) steps new-l new-z-pos (cycle-list-2 new-l new-pos steps))    
      ))
  
  )

(println (str "quiz 1: " (gen-memory 2017 1 343 [0] 0)))


(println (str "quiz 2:" (quiz-2 50000000 1 343 1 1 0)))
