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

(def example5 (mapv vec ["..........."
                         ".S-------7."
                         ".|F-----7|."
                         ".||.....||."
                         ".||.....||."
                         ".|L-7.F-J|."
                         ".|..|.|..|."
                         ".L--J.L--J."
                         "..........."]))

(def example5a (mapv vec [".........."
                          ".S------7."
                          ".|F----7|."
                          ".||....||."
                          ".||....||."
                          ".|L-7F-J|."
                          ".|..||..|."
                          ".L--JL--J."
                          ".........."]))

(def example6 (mapv vec [".F----7F7F7F7F-7...."
                         ".|F--7||||||||FJ...."
                         ".||.FJ||||||||L7...."
                         "FJL7L7LJLJ||LJ.L-7.."
                         "L--J.L7...LJS7F-7L7."
                         "....F-J..F7FJ|L7L7L7"
                         "....L7.F7||L7|.L7L7|"
                         ".....|FJLJ|FJ|F7|.LJ"
                         "....FJL-7.||.||||..."
                         "....L---J.LJ.LJLJ..."]))

(def example7 (mapv vec ["FF7FSF7F7F7F7F7F---7"
                         "L|LJ||||||||||||F--J"
                         "FL-7LJLJ||||||LJL-77"
                         "F--JF--7||LJLJ7F7FJ-"
                         "L---JF-JLJ.||-FJLJJ7"
                         "|F|F-JF---7F7-L7L|7|"
                         "|FFJF7L7F-JF7|JL---7"
                         "7-L-JL7||F7|L7F-7F7|"
                         "L.L7LFJ|||||FJL7||LJ"
                         "L7JLJL-JLJLJL--JLJ.L"]))

(defn read-maze [f]
  (mapv vec (str/split-lines (slurp (io/resource f)))))

(def data (read-maze "d10.txt"))

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

(defn tile? [maze pt] (= \. (get-in maze pt)))

(defn pipe? [maze pt] (contains? pipes (get-in maze pt)))

(defn connect-2 [maze a b]
  (let [v (mapv - b a)
        s (str/join (map #(get-in maze %) [a b]))]
    (contains? (connectors v) s)))

(defn connect-3 [maze a b c]
  (and (connect-2 maze a b)
       (connect-2 maze b c)))

(defn count-x [maze] (count (first maze)))

(defn count-y [maze] (count maze))

(defn cells [maze]
  (for [y (range (count-y maze))
        x (range (count-x maze))]
    [y x]))

(defn valid-point? [maze [y x]]
  (and (<= 0 y (dec (count-y maze)))
       (<= 0 x (dec (count-x maze)))))

(defn around [maze pt]
  (->> [[1 0] [-1 0] [0 1] [0 -1]]
       (map (partial mapv + pt))
       (filter (partial valid-point? maze))))

(defn set-cells [maze coll v]
  (mapv vec (reduce (fn [maze pt]
                      (assoc-in maze pt v))
                    maze coll)))

(defn replace-cells [maze match replacement]
  (mapv vec (reduce (fn [maze pt]
                      (if (= match (get-in maze pt))
                        (assoc-in maze pt replacement)
                        maze))
                    maze (cells maze))))

(defn find-animal [maze]
  (first (for [x (range (count-x maze))
               y (range (count-y maze))
               :when (= \S (get-in maze [y x]))]
           [y x])))

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

(defn flood-fill-loop [maze]
  (let [s (find-animal maze)
        p (infer-pipe maze s)
        maze (assoc-in maze s p)]
    (loop [step 0 queue (q/addq (q/emptyq) [s 0]) visited {}]
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

(defn hide-animal [maze]
  (let [a (find-animal maze)
        p (when a (infer-pipe maze a))]
    (if p
      (assoc-in maze a p)
      maze)))

(defn hide-pipes [maze]
  (let [pipes (filter (partial pipe? maze) (cells maze))]
    (set-cells maze pipes \space)))

(defn hide-junk [maze]
  (let [on-loop (set (map first (flood-fill-loop maze)))
        junk (remove on-loop (cells maze))]
    (set-cells maze junk \.)))

(defn expand-area [maze area pred]
  (loop [stack (seq area) visited #{}]
    (if-not (seq stack)
      visited
      (let [pt (first stack)
            coll (->> (around maze pt)
                      (remove visited)
                      (filter pred))]
        (recur (into (rest stack) coll)
               (conj visited pt))))))

(defn top-left-non-tile [maze]
  (let [maze-split (split-with #(every? #{\.} %) maze)
        row-split (split-with #{\.} (first (second maze-split)))]
    [(count (first maze-split))
     (count (first row-split))]))

(defn step-clock-wise [maze {:keys [cell dir inner]}]
  (let [debug false
        dir->vec {:north [-1 0] :south [1 0] :east [0 1] :west [0 -1]}
        cell-value (get-in maze cell)
        inner-dirs (if (#{\- \|} cell-value)
                     (get {:north [:east]
                           :east [:south]
                           :south [:west]
                           :west [:north]} dir)
                     (case [dir cell-value]
                       [:north \7] [:east :north]
                       [:south \L] [:west :south]
                       [:east \J] [:south :east]
                       [:west \F] [:north :west]
                       []))
        inner-cells (->> inner-dirs
                         (map #(mapv + cell (dir->vec %)))
                         (filter #(tile? maze %)))
        found-inner (expand-area maze inner-cells (partial tile? maze))
        next-dir (case cell-value
                   \- dir
                   \| dir
                   (case [dir cell-value]
                     [:north \F] :east
                     [:north \7] :west
                     [:east \7] :south
                     [:east \J] :north
                     [:west \L] :north
                     [:west \F] :south
                     [:south \L] :east
                     [:south \J] :west))
        next-cell (mapv + cell (dir->vec next-dir))
        _ (when debug
            (println :cell cell (get-in maze cell)
                     :dir dir
                     :inner-dirs inner-dirs))]
    {:cell next-cell
     :dir next-dir
     :inner (distinct (into inner found-inner))}))

(defn inner-area [maze]
  (let [start (top-left-non-tile maze)]
    (->> {:cell start :dir :north}
         (iterate (partial step-clock-wise maze))
         (drop 1)
         (take-while #(not= start (:cell %)))
         last
         :inner)))

(defn prettify-maze [maze]
  (let [pretty {\- \u2501
                \| \u2503
                \F \u250F
                \7 \u2513
                \L \u2517
                \J \u251B
                \I \u25CE
                \O \.}]
    (reduce (fn [maze pt]
              (let [v (get-in maze pt)
                    v (get pretty v v)]
                (assoc-in maze pt v)))
            maze (cells maze))))

(defn dump [maze]
  (->> maze
       prettify-maze
       (map (partial str/join ""))
       (run! println)))

(defn solve-1 [maze]
  (apply max (vals (flood-fill-loop maze))))

(defn solve-2 [maze]
  (-> maze
      hide-junk
      hide-animal
      inner-area
      count))

(println "part-1:" (solve-1 data))
(println "part-2:" (solve-2 data))
