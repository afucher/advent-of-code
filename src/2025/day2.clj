(ns day2
  (:require
   [clojure.string :as string]))

(def example (slurp "./src/2025/input-day2-ex.txt"))
(def input-1 (slurp "./src/2025/input-day2-1.txt"))

(def ranges
  (->> (-> input-1
           (clojure.string/replace "\n" "")
           (clojure.string/replace "\r" "")
           (clojure.string/split #","))
       (map #(clojure.string/split % #"-"))))

(defn composed-by [s id]
  (let [length (count s)
        id-length (count id)
        exact? (= 0 (mod id-length length))
        times (quot id-length length) #_2]

    (when exact?
      (= id
         (clojure.string/join (repeat times s))))))

(comment
  (clojure.string/join (repeat 5 "12"))

  (composed-by "12" "123123")
  (composed-by "12" "12")
  (composed-by "12" "1212")
  (composed-by "1" "112")
  (composed-by "1" "11")
  (composed-by "1" "111")

  (take 3 "123123")
  :rfc)

(defn invalid-id? [id]
  (->> (range 1 (inc (/ (count id) 2)))
       (map (fn [idx] (and
                       (> (count id) 1)
                       (composed-by (subs id 0 idx) id))))
       (some true?)))
(comment
  (invalid-id? "1")
  (invalid-id? "11")
  (invalid-id? "12")
  (invalid-id? "111")
  (invalid-id? "1111")
  (invalid-id? "1212")
  (invalid-id? "1188511885")
  (invalid-id? "38593859")
  :rfc)

(defn find-invalid-ids [start end]
  (let [ids (range start (inc end))]
    (reduce (fn [acc id]
              (if (invalid-id? (str id))
                (conj acc id)
                acc))
            [] ids)))
(invalid-id? (str 11))

(comment
  (find-invalid-ids 1 1)
  (find-invalid-ids 1 2)
  (find-invalid-ids 11 22)
  (find-invalid-ids 1188511880 1188511890)
  (find-invalid-ids 1698522 1698528)
  :rfc)

(def invalid-ids
  (->> ranges
       (map (fn [[start end]] (find-invalid-ids (parse-long start)
                                                (parse-long end))))))

(->> invalid-ids
     (remove empty?)
     flatten
     (reduce +))
