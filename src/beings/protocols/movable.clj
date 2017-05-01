(ns beings.protocols.movable)

(defprotocol Movable
  (move-by [self x-offset y-offset] "Moves the entity by the give offsets."))

(defn default-move-by [entity x-offset y-offset position-key]
  (update entity position-key
    (fn [[x y]] [(+ x x-offset)
                 (+ y y-offset)])))