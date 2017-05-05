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
(defrecord Grid [cells grid-dimensions area-dimensions]
  Object
  (toString [self] (format-grid self)))

(defn- index-of [x y width]
  (+ x (* y width)))

(defn- cell-index [grid x y]
  (index-of x y
            (first (:grid-dimensions grid))))

(defn- new-cells [width height]
  (vec (repeat (* width height) {})))

(defn- new-grid [grid-dimensions area-dimensions]
  (->Grid (apply new-cells grid-dimensions)
          grid-dimensions
          area-dimensions))

; TODO: Memoize?
; TODO: Figure out a better name.
(defn- grid-cell-real-area
  "Returns how much of the area each grid cell represents in each dimension."
  [grid-dimensions area-dimensions]
  (ph/div-pts area-dimensions grid-dimensions))

(defn- grid-cell-for-position
  "Returns which cell of the grid the position should occupy."
  [grid position]
  (let [{gd :grid-dimensions ad :area-dimensions} grid]
    (->> (grid-cell-real-area gd ad)
         (ph/div-pts position)
         (mapv int))))

(defn- position-matches? [pos1 pos2]
  (let [abs #(Math/abs ^double %)
        [x-diff y-diff] (mapv #(abs (- % %2)) pos1 pos2)]
    (and (< x-diff position-match-epsilon)
         (< y-diff position-match-epsilon))))

(defn add-positional [^Grid grid ^Positional positional]
  (let [pos (pP/get-position positional)
        [gx gy] (grid-cell-for-position grid pos)]
    (update-in grid [:cells (cell-index grid gx gy)] #(assoc % pos positional))))

(defn remove-positional [^Grid grid ^Positional positional]
  (let [{cells :cells} grid
        position (pP/get-position positional)
        [gw gy] (grid-cell-for-position grid position)]
    (update-in grid [:cells (cell-index grid gw gy)]
               #(dissoc % position))))

(defn- move-in-grid
  "If the new and old cell positions are the same, nothing happens, else the old positional is removed from it's current cell, and the new version is placed"
  [^Grid grid old-positional new-positional old-cell-pos new-cell-pos]
  (if (= old-cell-pos new-cell-pos)
    (let [old-pos (pP/get-position old-positional)
          new-pos (pP/get-position new-positional)]
      (a))

    (-> grid
        (remove-positional old-positional)
        (add-positional new-positional))))

(defn move-with-grid
  "Returns a pair of [new-grid new-positional] where the given positional has been moved to the new position, and whose new position is reflected in the returned new-grid."
  [^Grid grid ^Positional positional new-x new-y]
  (let [{cells :cells} grid
        current-position (pP/get-position positional)

        [cell-x cell-y :as grid-pos] (grid-cell-for-position grid current-position)
        [new-cell-x new-cell-y :as new-grid-pos] (grid-cell-for-position grid [new-x new-y])

        cell-i (cell-index grid cell-x cell-y)
        positional' (pP/set-position positional new-x new-y)

        grid' (move-in-grid grid positional positional' grid-pos new-grid-pos)]

    (if found-i?
      [(assoc-in grid [:cells cell-i found-i?] positional')
       positional']

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

(defn format-grid [^Grid grid]
  (let [cells (:cells grid)
        width (first (:grid-dimensions grid))]
    (clojure.string/join "\n"
       (mapv vec
             (partition width cells)))))