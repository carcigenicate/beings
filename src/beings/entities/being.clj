(ns beings.entities.being
  (:require [beings.protocols.has-health :as hP]
            [beings.protocols.movable :as mP]
            [beings.protocols.targetted :as tP]

            [helper]))

(defrecord Being [health position speed target])
