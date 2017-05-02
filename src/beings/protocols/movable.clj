(ns beings.protocols.movable)

(defprotocol Movable
  (move-by [self x-offset y-offset] "Moves the entity by the give offsets."))

(defn default-move-by [position-key entity x-offset y-offset]
  (update entity position-key
    (fn [[x y]] [(+ x x-offset)
                 (+ y y-offset)])))