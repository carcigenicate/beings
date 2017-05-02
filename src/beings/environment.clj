(ns beings.environment
  (:require [beings.entities.being :as b]

            [beings.protocols.positional :as pP]
            [beings.protocols.targetted :as tP]
            [beings.protocols.movable :as mP]
            [beings.protocols.has-health :as hP]

            [helpers.point-helpers :as ph]))

; TODO: Incorporate DNA that decides max health, speed, birthrate...

(def starting-health 100)

(def min-speed 2)
(def max-speed 5)

(defrecord Environment [beings food area-dimensions])

(defn new-beings [n-beings area-dimensions rand-gen]
  (let [[w h] area-dimensions]
    (for [n (range n-beings)]
      (b/->Being starting-health
                 (ph/random-point 0 w 0 h rand-gen)
                 (ph/random-point 0 w 0 h rand-gen)))))