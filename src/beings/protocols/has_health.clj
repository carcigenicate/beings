(ns beings.protocols.has-health)

(defprotocol Has-Health
  (heal [self by] "Heals the entity *by* the specified amount.")
  (hurt [self by] "Hurts the entity *by* the specified amount.")
  (health [self] "Returns the current entity health."))

(defn default-heal [entity by health-key]
  (update entity health-key #(+ % by)))

(defn default-hurt [entity by health-key]
  (default-heal entity (- by) health-key))