(ns beings.has-health)

(defprotocol Has-Health
  (heal [self by] "Heals the entity *by* the specified amount.")
  (hurt [self by] "Hurts the entity *by* the specified amount.")
  (health [self] "Returns the current entity health."))