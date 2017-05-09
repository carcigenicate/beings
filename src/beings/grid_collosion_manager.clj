(ns beings.grid-collosion-manager
  (:require [beings.protocols.positional :as pP]
            [helpers.point-helpers :as ph]
            [helpers.general-helpers :as g])

  (:import [beings.protocols.positional Positional]))

; TODO: Repeated calls to pP/get-position creates many vectors?
; TODO: Duplication for cell contents? Key holds data already held in the value.
; TODO: Doubles for map keys a problem? Floating point error may prevent it from being seen.

(def position-match-epsilon 0.001)

(declare format-grid)
(defrecord Grid [cells grid-side-length area-side-length]
  Object
  (toString [self] (format-grid self)))

(defn- index-of [x y width]
  (+ x (* y width)))

(defn- cell-index [grid x y]
  (index-of x y
            (:grid-side-length grid)))

(defn- get-cell
  "Returns the given cell. Will return null if either x or y is a non-integer."
  [grid x y]
  (get (:cells grid) (cell-index grid x y)))

(defn- new-cells [width height]
  (vec (repeat (* width height) [])))

(defn new-grid [grid-side-length area-side-length]
  (->Grid (new-cells grid-side-length grid-side-length)
          grid-side-length
          area-side-length))

(defn cant-find-positional-exception [positional operation-type]
  (IllegalStateException.
    (str "Tried to " operation-type " a Positional that wasn't already in the grid, "
         "or was previously moved incorrectly: " positional)))

; TODO: Figure out a better name.
(defn- grid-cell-real-area
  "Returns the ratio of the actual area to the grid area"
  [grid-side-length area-side-length]
  (/ grid-side-length area-side-length))

(defn grid-cell-for-position ; TODO: Re-privitize
  "Returns which cell of the grid the position should occupy."
  [grid position]
  (let [{grid-width :grid-side-length area-width :area-side-length} grid
        ratio (grid-cell-real-area grid-width area-width)]
    (mapv #(int (* % ratio)) position)))

(defn- grid-index-for-position [grid position]
  (apply cell-index grid
         (grid-cell-for-position grid position)))

(defn- position-matches? [pos1 pos2]
  (let [abs #(Math/abs ^double %)
        [x-diff y-diff] (mapv #(abs (- % %2)) pos1 pos2)]
    (and (< x-diff position-match-epsilon)
         (< y-diff position-match-epsilon))))

(defn- find-index-of-positional
  "Returns the first Positional found that matches the given position.
  Returns nil if no matching Positional is found."
  [cell-contents position]
  (reduce (fn [d [i posi]]
            (if (position-matches? (pP/get-position posi) position)
              (reduced i)
              d))
          nil
          (map vector (range) cell-contents)))

(defn add-positional [^Grid grid ^Positional positional]
  (let [pos (pP/get-position positional)
        [gx gy] (grid-cell-for-position grid pos)]
    (update-in grid [:cells (cell-index grid gx gy)] #(conj % positional))))

(defn add-positionals [^Grid grid positionals]
  (reduce add-positional grid positionals))

(defn- remove-from-vector [v i]
  (into (subvec v 0 i) (subvec v (inc i))))

(defn remove-positional [^Grid grid ^Positional positional]
  (let [{cells :cells} grid
        [x y :as pos] (pP/get-position positional)
        [grid-x grid-y] (grid-cell-for-position grid pos)
        cell-i (cell-index grid grid-x grid-y)
        cell (get cells cell-i)
        found-i? (find-index-of-positional cell pos)]
    (if found-i?
      (update-in grid [:cells cell-i]
                 #(remove-from-vector % found-i?))

      (throw (cant-find-positional-exception positional "remove")))))

(defn- move-in-cells [cells old-cell-i new-positional old-grid-position new-grid-position]
  (-> cells
      (update old-grid-position #(remove-from-vector % old-cell-i))
      (update new-grid-position #(conj % new-positional))))

; TODO: EWW
(defn move-with-grid
  "Returns a pair of [new-grid new-positional] where the given positional has been moved to the new position, and whose new position is reflected in the returned new-grid."
  [^Grid grid ^Positional positional new-x new-y]
  (let [{cells :cells} grid
        current-position (pP/get-position positional)

        grid-i (grid-index-for-position grid current-position)
        new-grid-i (grid-index-for-position grid [new-x new-y])
        
        found-i? (find-index-of-positional (get cells grid-i) current-position)]

    (if found-i?
      (let [positional' (pP/set-position positional new-x new-y)
            cells' (if (= grid-i new-grid-i)
                     (assoc-in grid [:cells new-grid-i found-i?] positional')
                     (move-in-cells cells found-i? positional' grid-i new-grid-i))]
        [(assoc grid :cells cells')
         positional'])

      (throw (cant-find-positional-exception positional "move")))))

; move-by-with-grid. Return the offset moved entity and the modified grid.
(defn move-by-with-grid
  "Returns a pair of [new-grid new-positional] where the given positional has been moved by the indicated offsets to the new position, and whose new position is reflected in the returned new-grid."
  [^Grid grid ^Positional positional x-offset y-offset]
  (let [[x y] (pP/get-position positional)]
    (move-with-grid grid positional (+ x x-offset)
                                    (+ y y-offset))))

(defn move-towards-by-with-grid
  "Returns a pair of [new-grid new-positional] where the given positional has been moved by the indicated amount to the given target position, and whose new position is reflected in the returned new-grid."
  [^Grid grid ^Positional positional target-x target-y by]
  (let [pos (pP/get-position positional)
        [x-off y-off] (pP/offsets-to-target pos [target-x target-y] by)]
    (move-by-with-grid grid positional x-off y-off)))

(defn inbounds? [grid-side-length x y]
  (and (< -1 x grid-side-length)
       (< -1 y grid-side-length)))

(defn surrounding-coords [grid-side-length grid-x grid-y depth]
  (for [y (range (- grid-y depth) (inc (+ grid-y depth)))
        x (range (- grid-x depth) (inc (+ grid-x depth)))
        :when (inbounds? grid-side-length x y)]
    [x y]))

(defn surrounding-cells [^Grid grid grid-x grid-y depth]
  (let [side-length (:grid-side-length grid)]
    (->> (surrounding-coords side-length grid-x grid-y depth)
      (map (fn [[x y]] (get-cell grid x y))))))

(defn filter-own-cell [cells position]
  (remove #(position-matches? (pP/get-position %) position)
          cells))

(defn grid-radius [^Grid grid search-radius]
  (let [{grid-width :grid-side-length area-width :area-side-length} grid]
    (int (Math/ceil ^double
                    (/ search-radius (/ area-width grid-width))))))

(defn search-cell-for-collisions [cell position search-radius]
  (filter #(let [pos (pP/get-position %)]
             (<= (ph/distance-between-pts pos position) search-radius))
          cell))

(defn search-cells-for-collisions [cells-to-check position search-radius]
  (reduce (fn [acc cell]
            (concat acc
                    (search-cell-for-collisions (filter-own-cell cell position)
                                                position search-radius)))
          '()
          cells-to-check))

(defn find-colliding [^Grid grid ^Positional positional search-radius]
  (let [{grid-width :grid-side-length} grid
        position (pP/get-position positional)
        [grid-x grid-y] (grid-cell-for-position grid position)
        g-radius (grid-radius grid search-radius)
        cells-to-search (surrounding-cells grid grid-x grid-y g-radius)]
    (search-cells-for-collisions cells-to-search position search-radius)))

(defn format-grid [^Grid grid]
  (let [cells (:cells grid)
        width (:grid-side-length grid)]
    (clojure.string/join "\n"
       (mapv vec
             (partition width
               (map #(map pP/get-position %) cells))))))

(def collider
  (pP/->Test-Positional 2 2))

(def test-grid
  (let [p pP/->Test-Positional
        a add-positional]
    (-> (new-grid 5 10)
      (a (p 1 1))
      (a (p 2 2))

      (a (p 9 9))

      (a (p 3 1))
      (a (p 5 7))
      (a (p 6 3)))))
