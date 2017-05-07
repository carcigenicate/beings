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
(defrecord Grid [cells grid-side-length area-dimensions]
  Object
  (toString [self] (format-grid self)))

(defn- index-of [x y width]
  (+ x (* y width)))

(defn- cell-index [grid x y]
  (index-of x y
            (:grid-side-length grid)))

(defn- new-cells [width height]
  (vec (repeat (* width height) [])))

(defn- new-grid [grid-side-length area-dimensions]
  (->Grid (new-cells grid-side-length grid-side-length)
          grid-side-length
          area-dimensions))

; TODO: Memoize?
; TODO: Figure out a better name.
(defn- grid-cell-real-area
  "Returns how much of the area each grid cell represents in each dimension."
  [grid-side-length area-dimensions]
  (mapv #(/ % grid-side-length) area-dimensions))

(defn- grid-cell-for-position
  "Returns which cell of the grid the position should occupy."
  [grid position]
  (let [{side-length :grid-side-length ad :area-dimensions} grid]
    (->> (grid-cell-real-area side-length ad)
         (ph/div-pts position)
         (mapv int))))

(defn- grid-index-for-position [grid position]
  (apply cell-index grid
         (grid-cell-for-position grid position)))

(defn- position-matches? [pos1 pos2]
  (let [abs #(Math/abs ^double %)
        [x-diff y-diff] (mapv #(abs (- % %2)) pos1 pos2)]
    (and (< x-diff position-match-epsilon)
         (< y-diff position-match-epsilon))))

(defn- matching-index [cell-contents position]
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
#_
(defn remove-positional [^Grid grid ^Positional positional]
  (let [{cells :cells} grid
        ()
        found-i? (matching-index)]))

(defn- remove-from-vector [v i]
  (into (subvec v 0 i) (subvec v (inc i))))

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
        
        found-i? (matching-index (get cells grid-i) current-position)]

    (if found-i?
      (let [positional' (pP/set-position positional new-x new-y)
            cells' (if (= grid-i new-grid-i)
                     (assoc-in grid [:cells new-grid-i found-i?] positional')
                     (move-in-cells cells found-i? positional' grid-i new-grid-i))]
        [(assoc grid :cells cells')
         positional'])

      (throw (IllegalStateException.
               (str "Tried to move a Positional that wasn't already in the grid, "
                    "or was previously moved incorrectly: " positional))))))

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

(defn inbounds? [^Grid grid x y]
  (let [width (:grid-side-length grid)]
    (and (< -1 x width)
         (< -1 y width))))

(defn cells-surrounding [^Grid grid x y depth]
  (for [y' (range (- y depth) (inc (+ y depth)))
        x' (range (- x depth) (inc (+ x depth)))
        :when (and (not (and (= x x') (= y y')))
                   (inbounds? grid x' y'))]
    [x' y']))

(defn find-colliding [^Grid grid ^Positional positional search-radius]
  (let [position (pP/get-position positional)
        [grid-x grid-y] (grid-cell-for-position grid position)
        grid-radius ()
        surrounding (cells-surrounding grid grid-x grid-y 1)]))

(defn format-grid [^Grid grid]
  (let [cells (:cells grid)
        width (:grid-side-length grid)]
    (clojure.string/join "\n"
       (mapv vec
             (partition width cells)))))

#_
(def test-grid
  (let [p pP/->Test-Positional
        p1 (p 99 99)]
    (-> (new-grid 10 [100 100])
      (add-positional p1))))
