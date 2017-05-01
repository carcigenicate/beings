(ns beings.protocols.targetted)

(defprotocol Targetted
  (get-target [self] "Returns the target of the entity as a vector pair.")
  (set-target [self x y]))