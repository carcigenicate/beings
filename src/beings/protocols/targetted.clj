(ns beings.protocols.targetted
  (:require [beings.protocols.movable :as mP]
            [beings.protocols.positional :as pP])

  (:import [beings.protocols.movable Movable]))

(def target-move-epsilon 0.01)

(defprotocol Targetted
  (get-target [self] "Returns the target of the entity as a vector pair.")
  (set-target [self x y]))
#_
(defn offsets-to-target [position target move-by]
  (let [[x-off y-off] (mapv - target position)
        angle (Math/atan (/ y-off x-off))]

    [(* move-by (Math/abs (Math/cos angle)) (Math/signum ^double x-off))
     (* move-by (Math/abs (Math/sin angle)) (Math/signum ^double y-off))]))

(defn offsets-to-target [position target move-by]
  (let [[x-off y-off] (mapv - target position)
        dist (Math/sqrt (+ (* y-off y-off) (* x-off x-off)))
        x-move (/ (* x-off move-by) dist)
        y-move (/ (* y-off move-by) dist)]

    (if (< dist move-by)
      [0 0]
      [x-move y-move])))

(defn move-to-target [movable-targetted-positional by]
  (let [pos (pP/get-position movable-targetted-positional)
        targ (get-target movable-targetted-positional)
        [x-off y-off] (offsets-to-target pos targ by)]
    (mP/move-by movable-targetted-positional x-off y-off)))

(defn x-primes [count]
  (let [x-factors (fn factors [n] (->> n range (map inc) (filter #(zero? (mod n %)))))
        x-prime? (fn [n] (->> (x-factors n) (count) (= 2)))]
    (->> (range)
         (filter x-prime?)
         (take count))))





