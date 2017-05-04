(ns beings.grid-collosion-manager
  (:require [beings.protocols.positional :as pP]
            [helpers.point-helpers :as ph]
            [helpers.general-helpers :as g])

  (:import [beings.protocols.positional Positional]))

(declare format-grid)
(defrecord Grid-Manager [cells grid-dimensions area-dimensions]
  Object
  (toString [self] (format-grid self)))

(defn index-of [x y width]
  (+ x (* y width)))

(defn cell-index [grid x y]
  (index-of x y
            (first (:grid-dimensions grid))))

(defn new-cells [width height]
  (vec (repeat (* width height) [])))

(defn new-grid [grid-dimensions area-dimensions]
  (let [[gw gh] grid-dimensions]
    (->Grid-Manager (new-cells gw gh)
                    grid-dimensions
                    area-dimensions)))

; TODO: Memoize?
; TODO: Figure out a better name.
(defn grid-cell-real-area
  "Returns how much of the area each grid cell represents in each dimension."
  [grid-dimensions area-dimensions]
  (ph/div-pts area-dimensions grid-dimensions))

(defn grid-cell-for-position
  "Returns which cell of the grid the position should occupy."
  [grid position]
  (let [{gd :grid-dimensions ad :area-dimensions} grid]
    (->> (grid-cell-real-area gd ad)
         (ph/div-pts position)
         (mapv int))))

(defn add-positional [grid ^Positional positional]
  (let [pos (pP/get-position positional)
        [gx gy] (grid-cell-for-position grid pos)]
    (update-in grid [:cells (cell-index grid gx gy)] #(conj % positional))))

; move-in-grid. Return just the moved grid
; move-with-grid. Return the moved entity, and the modified grid

; move-by-with-grid. Return the offset moved entity and the modified grid.

(defn format-grid [grid]
  (let [cells (:cells grid)
        width (first (:grid-dimensions grid))]
    (clojure.string/join "\n"
       (mapv vec
             (partition width cells)))))