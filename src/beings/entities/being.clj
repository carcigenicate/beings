(ns beings.entities.being
  (:require [beings.protocols.has-health :as hP]
            [beings.protocols.movable :as mP]
            [beings.protocols.positional :as pP]

            [helpers.general-helpers :as h]))

(defrecord Being [health position speed])

(extend Being
  hP/Has-Health
  {:heal (partial hP/default-heal :health)
   :hurt (partial hP/default-hurt :health)
   :health :health}

  mP/Movable
  {:move-by (partial mP/default-move-by :position)}

  pP/Positional
  {:get-position #(get % :position)
   :set-position #(assoc % :position [%2 %3])})



