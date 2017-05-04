(ns beings.main
  (:require [quil.core :as q]
            [quil.middleware :as m]

            [beings.entities.being :as b]
            [beings.environment :as e]

            [helpers.general-helpers :as g]
            [beings.protocols.positional :as pP])

  (:gen-class))

(def width 2500)
(def height 1500)

(def global-rand-gen (g/new-rand-gen 99))

(defrecord State [enviro])

(defn setup-state []
  {:beings (e/new-beings 20 [width height] global-rand-gen)})

(defn update-state [state]
  state)

(defn draw-state [state]
  (q/background 150 150 150)

  (let [beings (:beings state)]
    (doseq [being beings]
      (let [[x y] (:position being)]
        (q/ellipse x y 100 100)))))

(defn mouse-handler [state {x :x y :y}]
  (update state :beings
          (partial mapv
            #(pP/move-towards % [x y] (g/random-double 0 10 global-rand-gen)))))

(defn -main []
  (q/defsketch Beings-Test
               :size [width height]

               :setup setup-state
               :update update-state
               :draw draw-state

               :mouse-dragged mouse-handler

               :middleware [m/fun-mode]))
