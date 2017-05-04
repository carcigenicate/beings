(ns beings.protocols.positional)

(defprotocol Positional
  (get-position [self] "Returns the position as a vector pair.")
  (set-position [self x y] "Sets the entity's position."))

(defn move-by [positional x-offset y-offset]
  (let [[x y] (get-position positional)]
    (set-position positional
                  (+ x x-offset)
                  (+ y y-offset))))

(defn offsets-to-target [position target move-by]
  (let [[x-off y-off] (mapv - target position)
        dist (Math/sqrt (+ (* y-off y-off) (* x-off x-off)))
        x-move (/ (* x-off move-by) dist)
        y-move (/ (* y-off move-by) dist)]

    (if (< dist move-by)
      [0 0]
      [x-move y-move])))

(defn move-towards [movable-positional target by]
  (let [position (get-position movable-positional)
        [x-off y-off] (offsets-to-target position target by)]
    (move-by movable-positional x-off y-off)))