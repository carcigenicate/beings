(ns beings.environment
  (:require [beings.entities.being :as b]
            [beings.grid-collosion-manager :as cm]

            [beings.protocols.positional :as pP]
            [beings.protocols.movable :as mP]
            [beings.protocols.has-health :as hP]

            [helpers.point-helpers :as ph]
            [helpers.general-helpers :as g]))

; TODO: Incorporate DNA that decides max health, speed, birthrate...

(def starting-health 100)

(def min-speed 2)
(def max-speed 5)

(defrecord Environment [beings food area-dimensions])

(defn new-beings [n-beings area-dimensions rand-gen]
  (let [[w h] area-dimensions]
    (vec
      (for [n (range n-beings)]
        (b/->Being starting-health
                   (ph/random-point 0 w 0 h rand-gen)
                   (g/random-double min-speed max-speed rand-gen))))))

(defn new-environment [dimensions]
  (->Environment [] [] dimensions))

(defn random-populate [enviro n-beings rand-gen]
  (let [bs (new-beings n-beings (:area-dimensions enviro) rand-gen)]
    (assoc enviro :beings bs)))

(defn closest-positional [current-positional other-positionals]
  (let [cur-pos (pP/get-position current-positional)]
    (reduce (fn [[_ cl-dist :as old] o-pos]
              (let [dist (ph/distance-between-pts cur-pos (pP/get-position o-pos))]
                (if (< dist cl-dist)
                  [o-pos dist]
                  old)))
            [nil Long/MAX_VALUE]
            other-positionals)))

