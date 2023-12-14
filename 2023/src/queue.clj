(ns queue
  (:require [clojure.string :as str]))

(defrecord Queue [head tail front count]
  Object
  (toString [_] (format "<%s>"
                        (str/join " " (concat (reverse tail) head)))))

(defn emptyq [] (->Queue nil nil nil 0))

(defn addq [q x]
  (cond-> q
    (zero? (:count q)) (assoc :front x)
    true (update :count inc)
    true (update :tail conj x)))

(defn popq [q]
  (cond
    (not (seq (:head q)))
    (let [head (rest (reverse (:tail q)))]
      (-> q
          (update :count dec)
          (assoc :head head :tail nil :front (first head))))

    (not (seq (rest (:head q))))
    (let [head (reverse (:tail q))]
      (-> q
          (update :count dec)
          (assoc :head head :tail nil :front (first head))))

    :else
    (-> q
        (update :count dec)
        (update :head rest)
        (assoc :front (second (:head q))))))

(defn frontq [q]
  (:front q))

(defn countq [q]
  (:count q))

