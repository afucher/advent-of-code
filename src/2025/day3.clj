(ns day3
  (:require
   [clojure.string :as string]))

(def example (slurp "./src/2025/input-day3-ex.txt"))
(def input-1 (slurp "./src/2025/input-day3-1.txt"))

(def banks (-> input-1
               (string/split-lines)))

(defn find-max-joltage-2 [bank]
  (let [bank (map Character/getNumericValue (seq bank))
        high (apply max (butlast bank))
        idx (.indexOf bank high)
        next-high (apply max (drop (inc idx) bank))]
    [high next-high]))

(comment
  (find-max-joltage-2  "987654321111111")

  (->> banks
       (map find-max-joltage-2)
       (map #(apply str %))
       (map parse-long)
       (reduce +))
  :rfc)


(defn find-max-from-bank [bank n]
  (let [bank (map Character/getNumericValue (seq bank))
        high (apply max (drop-last (dec n) bank))
        idx (.indexOf bank high)]
    [high idx]))

(defn find-max-joltage [number-of-batteries bank]
  (->> (reduce
        (fn [acc n]
          (let [last-idx (apply + (map (comp inc second) acc))
                new-bank (subs bank last-idx)]
            (conj acc (find-max-from-bank new-bank n))))
        []
        (range number-of-batteries 0 -1))
       (map first)))

(comment

  (def find-max-joltage-2-batteries (partial find-max-joltage 2))
  (->> banks
       (map find-max-joltage-2-batteries)
       (map #(apply str %))
       (map parse-long)
       (reduce +))



  (def find-max-joltage-12 (partial find-max-joltage 12))

  (->> banks
       (map find-max-joltage-12)
       (map #(apply str %))
       (map parse-long)
       (reduce +))
  :rfc)
