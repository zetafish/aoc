(ns d10
  "Pipe Maze"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [queue :as q]))

(def example (mapv vec ["....."
                        ".F-7."
                        ".|.|."
                        ".L-J."
                        "....."]))

(def example2 (mapv vec ["....."
                         ".S-7."
                         ".|.|."
                         ".L-J."
                         "....."]))

(def example3 (mapv vec ["..F7."
                         ".FJ|."
                         "SJ.L7"
                         "|F--J"
                         "LJ..."]))

(def example4 (mapv vec ["7-F7-"
                         ".FJ|7"
                         "SJLL7"
                         "|F--J"
                         "LJ.LJ"]))

(def pipes {\| (set [:n :s])
            \- (set [:e :w])
            \F (set [:s :e])
            \J (set [:w :n])
            \7 (set [:s :w])
            \L (set [:n :e])})

(def connectors
  (->> (for [p pipes
             q pipes
             [v d1 d2] [[[1 0] :s :n]
                        [[-1 0] :n :s]
                        [[0 1] :e :w]
                        [[0 -1] :w :e]]
             :when (and (contains? (second p) d1)
                        (contains? (second q) d2))]
         [v (str/join (map first [p q]))])
       (group-by first)
       (map (fn [[v coll]] [v (set (map second coll))]))
       (into {})))

(defn connect-2 [maze a b]
  (let [v (mapv - b a)
        s (str/join (map #(get-in maze %) [a b]))]
    (contains? (connectors v) s)))

(defn connect-3 [maze a b c]
  (and (connect-2 maze a b)
       (connect-2 maze b c)))

(defn read-maze [f]
  (mapv vec (str/split-lines (slurp (io/resource f)))))

(def data (read-maze "d10.txt"))

(defn dump [maze]
  (run! println maze))

(defn count-x [maze] (count (first maze)))

(defn count-y [maze] (count maze))

(defn valid-point? [maze [y x]]
  (and (<= 0 y (inc (count-y maze)))
       (<= 0 x (inc (count-x maze)))))

(defn find-animal [maze]
  (first (for [x (range (count-x maze))
               y (range (count-y maze))
               :when (= \S (get-in maze [x y]))]
           [x y])))

(defn around [maze pt]
  (->> [[1 0] [-1 0] [0 1] [0 -1]]
       (map (partial mapv + pt))
       (filter (partial valid-point? maze))))

(defn triples [maze pt]
  (let [coll (around maze pt)]
    (->> (for [a coll
               b coll
               :when (neg? (compare a b))]
           [a pt b]))))

(defn infer-pipe [maze pt]
  (->> (for [p (keys pipes) t (triples maze pt)] [p t])
       (filter (fn [[p [l m r]]] (connect-3 (assoc-in maze m p) l m r)))
       ffirst))

(defn flood-fill [maze]
  (let [s (find-animal maze)
        p (infer-pipe maze s)
        maze (assoc-in maze s p)]
    (loop [step 0 queue (q/addq (q/emptyq) [s 0]) visited {}]
      ;; (println queue "keys"(keys visited))
      (if (zero? (q/countq queue))
        visited
        (let [[pt n] (q/frontq queue)
              candidates (->> (around maze pt)
                              (remove visited)
                              (filter #(connect-2 maze pt %))
                              (map #(vector % (inc n))))]
          (recur (inc step)
                 (reduce q/addq (q/popq queue) candidates)
                 (assoc visited pt n)))))))

(defn solve-1 [maze]
  (apply max (vals (flood-fill maze))))

(solve-1 example2)
(solve-1 example3)
(solve-1 data)
