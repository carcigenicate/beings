(ns beings.entities.being
  (:require [beings.protocols.has-health :as hP]
            [beings.protocols.movable :as mP]
            [beings.protocols.targetted :as tP]
            [beings.protocols.positional :as pP]

            [helpers.general-helpers :as h]))

(defrecord Being [health position speed target])

(extend Being
  hP/Has-Health
  {:heal (partial hP/default-heal :health)
   :hurt (partial hP/default-hurt :health)
   :health :health}

  mP/Movable
  {:move-by (partial mP/default-move-by :position)}

  tP/Targetted
  {:get-target #(get % :target)
   :set-target #(assoc % :target [%2 %3])}

  pP/Positional
  {:get-position #(get % :position)
   :set-position #(assoc % :position [%2 %3])})



