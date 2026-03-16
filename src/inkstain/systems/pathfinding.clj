(ns inkstain.systems.pathfinding
  (:require [clojure.pprint :as pprint]
            [clojure.data.priority-map :refer [priority-map priority-map-by]]
            [inkstain.systems.grid :as grid]))



(defn manhattan-distance [[x1 y1] [x2 y2]]
  (+ (Math/abs ^Integer (- x2 x1)) (Math/abs ^Integer (- y2 y1))))

(defn cost [curr start end]
  (let [g (manhattan-distance start curr)
        h (manhattan-distance curr end)
        f (+ g h)]
    [f g h]))

(defn edges
  "Generates new nodes on the frontier"
  [grid width height closed [x y]]
  (for [tx (range (- x 1) (+ x 2))
        ty (range (- y 1) (+ y 2))
        :when (and (>= tx 0)
                (>= ty 0)
                (<= tx width)
                (<= ty height)
                (not= [x y] [tx ty])
                (grid/walkable? grid tx ty)
                (not (contains? closed [tx ty])))]
    [tx ty]))

(defn path [end parent closed]
  (vec
    (reverse
      (loop [path [end parent]
             node (closed parent)]
        (cond
          (or (nil? parent) (= end parent)) []

          (nil? node) path

          :else
          (recur (conj path node) (closed node)))))))

;; TODO(#1): Need to add 3d support
(defn search
  ([grid start end]
   (let [[sx sy] start
         [ex ey] end
         {:keys [width height]} grid
         open (priority-map-by
                (fn [x y]
                  (if (= x y)
                    0
                    (let [[f1 _ h1] x
                          [f2 _ h2] y]
                      (if (= f1 f2)
                        (if (< h1 h2) -1 1)
                        (if (< f1 f2) -1 1)))))
                start (cost start start end))
         closed {}
         ;; prevent right and bottom most edges from being considered as out of bounds
         width' (dec width)
         height' (dec height)]

     ;; we check start / end validity
     (search grid width' height' open closed start end)))

  ([grid width height open closed start end]
   (when-let [[coord [_ _ _ parent]] (peek open)]
     (if-not (= coord end)
       (let [closed (assoc closed coord parent)
             edges (edges grid width height closed coord)
             open (reduce
                    (fn [open edge]
                      (if (not (contains? open edge))
                        (assoc open edge (conj (cost edge start end) coord))
                        (let [[_ pg] (open edge)
                              [nf ng nh] (cost edge start end)]
                          (if (< ng pg)
                            (assoc open edge (conj [nf ng nh] coord))
                            open))))
                    (pop open) edges)]
         (recur grid width height open closed start end))
       (path end parent closed)))))

(defn try-search
  ([grid start end] (try-search grid start end {:check-start? true :check-end? true}))
  ([grid start end {:keys [check-start? check-end?] :or {check-start? true check-end? true}}]
   (let [[sx sy] start
         [ex ey] end
         start? (grid/walkable? grid sx sy)
         end? (grid/walkable? grid ex ey)]
     (when
       (or
         (and check-start? (not check-end?) start?)
         (and check-end? (not check-start?) end?)
         (and check-start? check-end? start? end?))
       (search grid start end)))))

(defn loose-search
  "only check if the end is walkable"
  [grid start end]
  (try-search grid start end {:check-end? true}))

(defn draw-grid
  ([grid start end] (draw-grid grid start end {}))
  ([grid start end opts]
   (let [path (time (try-search grid start end opts))
         draw-path (into #{} path)
         _ (println "Path:")
         _ (pprint/pprint path)
         {:keys [width height]} grid
         area (map
                (fn [y]
                  (map
                    (fn [x]
                      (let [walkable? (grid/walkable? grid x y)]
                        ;;(println idx walkable? width height)
                        ;;(println x y)
                        ;;(println :idx idx (grid/cell-idx x y width))
                        (cond
                          (contains? draw-path [x y]) \x
                          (not walkable?) \#
                          :else \space)))
                    (range width)))
                (range height))]

     (doseq [line area]
       (println line)))))

(defn create-grid
  ([width length]
   (vec (repeat length
          (vec (repeat width 0)))))
  ([width length height]
   (vec (repeat height
          (vec (repeat length
                 (vec (repeat width 0))))))))

;; maze = easy to read format
;; grid for pathfinding

(defn maze->grid [maze]
  (let [height (count maze)
        width (count (first maze))]
    {:width width :height height
     :walkable (into [] (comp cat (map (fn [x] (= x 0)))) maze)}))

(defn draw-maze
  ([maze start end] (draw-maze maze start end {}))
  ([maze start end opts]
   (let [grid (maze->grid maze)
         path (time (try-search grid start end opts))
         draw-path (into #{} path)
         _ (println "Path:")
         _ (pprint/pprint path)
         area (map-indexed
                (fn [idx-row row]
                  (map-indexed
                    (fn [idx-col col]
                      (cond
                        (contains? draw-path [idx-col idx-row]) \x
                        (= 1 col) \#
                        :else \space))
                    row))
                maze)]

     (doseq [line area]
       (println line)))))

(comment
  (create-grid 5 4)
  ;; TODO(#1): extend to 3D
  (create-grid 5 4 2))


;; Maze
(def maze1 [[0 0 0 0 0 0 0]
            [0 0 0 1 0 0 0]
            [0 0 0 1 0 0 0]
            [0 0 0 1 0 0 0]
            [0 0 0 0 0 0 0]])

#_
(draw-maze maze1 [1 2] [5 2])

(def maze2 [[0 0 0 0 0 0 0]
            [0 0 1 1 1 0 0]
            [0 0 0 1 0 0 0]
            [0 0 0 1 0 0 0]
            [0 0 0 1 0 0 0]])
#_
(draw-maze maze2 [1 3] [5 2])

(def maze3 [[0 1 0 0 0 1 0]
            [0 1 0 1 0 1 0]
            [0 1 0 1 0 1 0]
            [0 1 0 1 0 1 0]
            [0 0 0 1 0 0 0]])
#_
(draw-maze maze3 [0 0] [6 0])

(def maze4 [[0 0 0 0 0 0 0 0]
            [1 1 1 1 1 1 1 0]
            [0 0 0 1 0 0 0 0]
            [0 0 0 1 0 0 0 0]
            [0 0 0 1 0 0 0 0]
            [0 0 0 1 1 1 0 1]
            [0 0 0 0 0 1 0 1]
            [0 0 0 0 0 1 0 1]
            [0 0 0 0 0 0 0 1]
            [1 1 1 1 0 1 1 1]
            [0 0 0 1 0 0 0 0]
            [0 0 0 1 0 0 0 0]
            [0 0 0 0 0 0 0 0]])
#_
(draw-maze maze4 [0 0] [0 12])

;; 5x5 grid with a wall forcing the path along the right edge
(def maze5
  [[0 0 0 0 0]
   [0 1 1 1 0]
   [0 1 0 1 0]
   [0 1 1 1 0]
   [0 0 0 0 0]])
#_ ;; Should find a path along x=4 (the rightmost column)
(draw-maze maze5 [0 0] [4 4])

#_ ;; Path should go straight down the right edge
(draw-maze maze5 [4 0] [4 4])

#_ ;; Must go around the wall — the only route uses the edge columns/rows
(draw-maze maze5 [0 0] [4 2])

#_ ;; Goal at the very corner
(draw-maze maze5 [0 0] [4 0])

;; 2x1 grid starting not walkable to check it works if start is not walkable
(def maze6
  [[1 0]])

#_ ;; it's not pathfinding's job to see if it can traverse the start
(draw-maze maze6 [0 0] [1 0] {:check-start? false})
