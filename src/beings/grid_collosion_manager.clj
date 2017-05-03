(ns beings.grid-collosion-manager
  (:require [beings.protocols.positional :as pP]
            [helpers.point-helpers :as ph]
            [helpers.general-helpers :as g])

  (:import [beings.protocols.positional Positional]))

(defrecord Grid-Manager [cells grid-dimensions area-dimensions])

(defn new-grid [grid-dimensions area-dimensions]
  (let [[gw gh] grid-dimensions]
    (->Grid-Manager (repeat (* gw gh) nil)
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
        grid-pos (grid-cell-for-position grid positional)]))
