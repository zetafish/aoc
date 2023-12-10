(ns d6
  "Wait For It")

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

(defn beat-record [time record]
  (->> (map #(race % time) (range (inc time)))
       (filter #(> % record))))

(defn ways-to-beat-all
  [{:keys [time dist]}]
  (reduce * (map #(count (apply beat-record %))
                 (partition 2 (interleave time dist)))))

(ways-to-beat-all example)
(ways-to-beat-all data)
