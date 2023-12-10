(ns d6
  "Wait For It"
  (:require [clojure.string :as str]))

(def example
  {:time [7 15 30]
   :dist [9 40 200]})

(def data
  {:time [42 68 69 85]
   :dist [284 1005 1122 1341]})

{:time 0 :speed 0}

(defn race [button-time race-time]
  (cond
    (zero? button-time) 0
    (= button-time race-time) 0
    :else (* button-time (- race-time button-time))))

(defn beat-count [time record]
  (- (inc time)
     (count (take-while #(<= (race % time) record) (range (inc time))))
     (count (take-while #(<= (race % time) record) (range time -1 -1)))))

(defn count-the-ways-1 [{:keys [time dist]}]
  (reduce *
          (map #(apply beat-count %)
               (partition 2 (interleave time dist)))))

(defn count-the-ways-2 [{:keys [time dist]}]
  (beat-count
   (Long/parseLong (str/join (map str time)))
   (Long/parseLong (str/join (map str dist)))))

(count-the-ways-1 example)
(count-the-ways-1 data)

(count-the-ways-2 example)
(count-the-ways-2 data)

