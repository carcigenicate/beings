(ns beings.protocols.movable
  (:require [beings.protocols.positional :as pP]))

(defprotocol Movable
  (move-by [self x-offset y-offset] "Moves the entity by the give offsets."))

(defn default-move-by [position-key entity x-offset y-offset]
  (update entity position-key
    (fn [[x y]] [(+ x x-offset)
                 (+ y y-offset)])))

(defn offsets-to-target [position target move-by]
  (let [[x-off y-off] (mapv - target position)
        dist (Math/sqrt (+ (* y-off y-off) (* x-off x-off)))
        x-move (/ (* x-off move-by) dist)
        y-move (/ (* y-off move-by) dist)]

    (if (< dist move-by)
      [0 0]
      [x-move y-move])))

(defn move-towards [movable-positional target by]
  (let [position (pP/get-position movable-positional)
        [x-off y-off] (offsets-to-target position target by)]
    (move-by movable-positional x-off y-off)))