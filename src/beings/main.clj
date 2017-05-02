(ns beings.main
  (:require [quil.core :as q]
            [quil.middleware :as m]

            [beings.entities.being :as b]
            [beings.protocols.targetted :as tP])

  (:gen-class))

(def width 1000)
(def height 1000)

(defn setup-state []
  {:being (b/->Being 100 [(/ width 2) (/ height 2)]
                     5 nil)})

(defn update-state [state]
  (update state :being
    #(if (:target %)
       (tP/move-to-target % (:speed %))
       %)))

(defn draw-state [state]
  (q/background 150 150 150)
  (let [{{[x y] :position} :being} state]
    (q/ellipse x y 100 100)))

(defn mouse-handler [state event]
  (let [{x :x y :y} event]
    (update state :being
      #(tP/set-target % x y))))

(defn -main []
  (q/defsketch Beings-Test
               :size [width height]

               :setup setup-state
               :update update-state
               :draw draw-state

               :mouse-clicked mouse-handler

               :middleware [m/fun-mode]))
