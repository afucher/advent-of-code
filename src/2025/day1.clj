(ns day1
  (:require
   [clojure.string :as string]))
(def example (slurp "./src/2025/input-day1-ex.txt"))
(def input-1 (slurp "./src/2025/input-day1-1.txt"))

(def instructions
  (-> input-1
      clojure.string/split-lines))

(subs "R33" 1)

;; 50 R33 -> 50 + 33
(defn rotate [n s]
  (if (= (first s) \R)
    (+ n (parse-long (subs s 1)))
    (- n (parse-long (subs s 1)))))

(comment
  (rotate 50 "R33")
  (rotate 50 "L33")
  :rfc)

(defn normalize [n]
  (let [m (mod n 100)]
    (cond
      (< m 0) (+ m 100)
      (>= m 100) (- m 100)
      :else m)))

(comment
  (normalize 33)
  (normalize 117)
  (normalize -4)
  (normalize 100)
  (normalize -400)
  (normalize -401)
  (normalize 400)
  (normalize 401)
  :rfc)
(def starts-point 50)

(->> instructions
     (reduce (fn [acc el]
               (conj acc (normalize (rotate (last acc) el))))
             [starts-point])
     #_(map println)
     (filter #(= %1 0))

     count)

(comment
  (-> (update {:stack [starts-point]
               :zeros 0}
              :zeros
              #(+ 4 %))
      (update :stack conj 1))

  (quot 401 100)
  (quot -401 100)
  (quot 99 100)
  :rfc)

(let [{:keys [stack zeros]}
      (reduce (fn [acc el]
                (let [last-pos (-> acc :stack last)
                      new-pos (rotate last-pos el)
                      normalized-pos (normalize new-pos)
                      changed-direction (and (not (zero? new-pos))
                                             (or (and (pos? new-pos) (neg? last-pos))
                                                 (and (pos? last-pos) (neg? new-pos))))
                      n-zeros (if (= new-pos 100)
                                0
                                (let [times-greater (abs (quot new-pos 100))
                                      zeros (if changed-direction
                                              (inc times-greater)
                                              times-greater)]
                                  (if (and (> zeros 0)
                                           (= 0 normalized-pos))
                                    (dec zeros)
                                    zeros)))
                      _ (println changed-direction last-pos new-pos (normalize new-pos) n-zeros)]
                  (-> acc
                      (update :stack conj (normalize new-pos))
                      (update :zeros #(+ n-zeros %)))))
              {:stack [starts-point]
               :zeros 0}
              instructions)
      stack-zeros (count (filter #(= 0 %1) stack))]
  (println stack)
  (println stack-zeros)
  (+ stack-zeros zeros))

