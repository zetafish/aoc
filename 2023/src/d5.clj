(ns d5
  "If You Give A Seed A Fertilizer"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn parse-int [s] (Long/parseLong s))

(defn parse-seeds [[seeds]]
  (map parse-int
       (str/split
        (second (str/split seeds #"seeds: "))
        #" +")))

(defn parse-mapping [lines]
  (map (comp (partial map parse-int)
             #(str/split % #" +")) lines))

(defn read-almanac [f]
  (let [sections (->> (str/split-lines (slurp (io/resource f)))
                      (partition-by #{""})
                      (remove #{[""]})
                      vec)]
    {:seeds (parse-seeds (sections 0))
     :mappings (map (comp parse-mapping rest) (rest sections))}))

(defn match-range [[s-min s-length] [m-min m-length]]
  (let [s-max (+ s-min s-length -1)
        m-max (+ m-min m-length -1)
        valid? (partial apply <=)
        encode (fn [[lo hi]] [lo (inc (- hi lo))])]
    {:miss (->> [[s-min (min s-max (dec m-min))]
                 [(max s-min (inc m-max)) s-max]]
                (filter valid?)
                (map encode))
     :hit (->> [[(max m-min s-min) (min m-max s-max)]]
               (filter valid?)
               (map encode))}))

(defn apply-mapping-entry [rn [dst src  n]]
  (let [{:keys [hit miss]} (match-range rn [src n])]
    {:miss miss
     :hit (map (fn [[s n]] [(+ s dst (- src)) n]) hit)}))

(defn apply-mapping [ranges mapping]
  (apply concat
         (vals
          (reduce (fn [state entry]
                    (let [coll (map #(apply-mapping-entry % entry) (:miss state))]
                      (-> state
                          (update :hit concat (mapcat :hit coll))
                          (assoc :miss (mapcat :miss coll)))))
                  {:miss ranges}
                  mapping))))

(defn min-location [almanac]
  (->> (reduce apply-mapping
               (:seeds almanac)
               (:mappings almanac))
       (map first)
       (apply min)))

(def example (read-almanac "d5_small.txt"))
(def data (read-almanac "d5.txt"))

;; part 1
(min-location (update example :seeds zipmap (repeat 1)))
(min-location (update data :seeds zipmap (repeat 1)))

;; part 2
(min-location (update example :seeds (partial partition 2)))
(min-location (update data :seeds (partial partition 2)))
