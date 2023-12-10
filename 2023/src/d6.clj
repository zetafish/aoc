(ns d6
  "Wait For It"
  (:require [clojure.string :as str]))

(def example
  {:time [7 15 30]
   :dist [9 40 200]})

(def data
  {:time [42 68 69 85]
   :dist [284 1005 1122 1341]})

(defn race [race-time button-time]
  (* button-time (- race-time button-time)))

(defn bin-lsearch
  "Find lowest m in [a..b] for which (> (f m) r)"
  [a b f r]
  ;; (println (format "%s:%s %s:%s" a (f a) b (f b)))
  (cond
    (= a b) b
    (= (inc a) b) (if (> (f a) r) a b)
    :else (let [m (quot (+ a b) 2)]
            (if (<= (f m) r)
              (recur m b f r)
              (recur a m f r)))))

(defn bin-rsearch
  "Find highest m in [a..b] for which (> (f m r)"
  [a b f r]
  ;; (println (format "%s:%s %s:%s" a (f a) b (f b)))
  (cond
    (= a b) b
    (= (inc a) b) (if (> (f b) r) b a)
    :else (let [m (quot (+ a b) 2)]
            (if (<= (f m) r)
              (recur a m f r)
              (recur m b f r)))))

(defn record-breakers [race-time record]
  (let [m (quot race-time 2)
        f (partial race race-time)
        a (bin-lsearch 0 m f record)
        b (bin-rsearch m race-time f record)]
    [a b]))

(defn count-record-breakers [race-time record]
  (let [[a b] (record-breakers race-time record)]
    (inc (- b a))))

(defn part-1 [{:keys [time dist]}]
  (let [races (partition 2 (interleave time dist))]
    (reduce * (map #(apply count-record-breakers %) races))))

(defn part-2 [{:keys [time dist]}]
  (let [f (fn [coll] (Long/parseLong (str/join (map str coll))))]
    [(f time)
     (f dist)]
    (count-record-breakers (f time) (f dist))))

(part-1 example)
(part-1 data)

(part-2 example)
(part-2 data)

