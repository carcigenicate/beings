(ns beings.protocols.positional)

(defprotocol Positional
  (get-position [self] "Returns the position as a vector pair.")
  (set-position [self x y] "Sets the entity's position."))