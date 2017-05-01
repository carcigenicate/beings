(ns beings.entities.being
  (:require [beings.protocols.has-health :as hP]
            [beings.protocols.movable :as mP]
            [beings.protocols.targetted :as tP]

            [helpers.general-helpers :as h]))

(defrecord Being [health position speed target])

(extend Being
  hP/Has-Health
  {:heal hP/default-heal
   :hurt hP/default-hurt
   :health :health}

  mP/Movable
  {:move-by mP/default-move-by})



