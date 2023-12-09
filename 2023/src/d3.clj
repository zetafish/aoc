(ns d3
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def digit? (comp boolean (set "0123456789")))

(def engine-symbol? (complement (set ".0123456789")))

(def engine (atom nil))

(defn read-engine [f]
  (mapv vec (str/split-lines (slurp (io/resource f)))))

(defn in-range [engine [y x]]
  (and (<= 0 y) (< y (count engine))
       (<= 0 x) (< x (count (first engine)))))

(defn find-digits [y s]
  (let [parts (->> s
                   (partition-by (complement digit?))
                   (map str/join))
        indices (reductions + 0 (map count  parts))]
    (->>
     (interleave parts indices (rest indices))
     (partition 3)
     (filter (comp digit? ffirst))
     (map (fn [[n x1 x2]] {:n (Long/parseLong n) :y y :x1 x1 :x2 x2}))
     seq)))

(defn around [engine y x1 x2]
  (filter #(in-range engine %)
          (concat (map #(vector (dec y) %) (range (dec x1) (inc x2)))
                  (list [y (dec x1)] [y x2])
                  (map #(vector (inc y) %) (range (dec x1) (inc x2))))))

(defn sum-of-the-parts [engine]
  (->> engine
       (map-indexed find-digits)
       (apply concat)
       (map (fn [{:keys [y x1 x2] :as m}]
              (let [border (around engine y x1 x2)]
                (-> m
                    (assoc :border border
                           :symbols (->> border
                                         (map #(get-in engine %))
                                         (filter engine-symbol?)
                                         seq))))))
       (filter :symbols)
       (map :n)
       (reduce +)))

(sum-of-the-parts (read-engine "d3_small.txt"))
(sum-of-the-parts (read-engine "d3.txt"))
