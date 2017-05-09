(ns beings.main
  (:require [quil.core :as q]
            [quil.middleware :as m]

            [beings.entities.being :as b]
            [beings.environment :as e]
            [beings.protocols.positional :as pP]
            [beings.grid-collosion-manager :as col]

            [helpers.general-helpers :as g]
            [helpers.quil-helpers :as qh]
            [helpers.point-helpers :as ph])

  (:gen-class))

(def width 1500)
(def height 1500)

(def grid-width 20)

(def being-size 100)
(def pop-size 100)

(def grid-spacing (/ width grid-width))

(def global-rand-gen (g/new-rand-gen 99))

(defrecord State [enviro grid])

(defrecord Colored-Being [being color]
  pP/Positional
  (get-position [_] (pP/get-position being))
  (set-position [self x y] (assoc self :being (pP/set-position being x y))))

(defn new-colored-beings [n-beings area-dimensions rand-gen]
  (let [[w h] area-dimensions]
    (vec
      (for [_ (range n-beings)]
        (->Colored-Being
          (b/->Being 100
                     (ph/random-point 0 w 0 h rand-gen)
                     (g/random-double 1 20 rand-gen))
          [255 255 255])))))

(defn setup-state []
  (let [g (col/new-grid grid-width width)
        bs (new-colored-beings pop-size [width height] global-rand-gen)
        g' (col/add-positionals g bs)]

    {:beings bs
     :grid g'}))

(defn resolve-collision [grid being]
  (let [colliders (col/find-colliding grid being being-size)]
    (if (empty? colliders)
      being
      (assoc being :color (qh/random-color global-rand-gen)))))

(defn resolve-collisions [grid beings]
  (mapv #(resolve-collision grid %) beings))

(defn update-state [state]
  (when (zero? (rem (q/frame-count) 150))
    (println (q/current-frame-rate)))

  (-> state
    (update :beings #(resolve-collisions (:grid state) %))))

(defn draw-beings [beings]
  (doseq [being beings]
    (let [[x y] (pP/get-position being)
          c (:color being)]
      (q/with-fill c
        (q/ellipse x y being-size being-size)))))

(defn draw-grid []
  (qh/with-weight 2
      (doseq [y (range 0 (inc width) grid-spacing)
              x (range 0 (inc width) grid-spacing)]
        (q/with-translation [x y]
          (q/line 0 0 grid-spacing 0)
          (q/line 0 0 0 grid-spacing)))))

(defn draw-state [state]
  (q/background 150 150 150)

  (let [{beings :beings, grid :grid} state
        radius (col/grid-radius grid being-size)
        real-dist (* (+ radius 2) grid-spacing)]

    (draw-beings beings)
    (draw-grid)))

(defn mouse-handler [state {x :x y :y}]
  (let [{grid :grid} state
        g (atom grid)]
    #_
    (-> state
      (update :beings
              (partial mapv
                       ; TODO: Figure out why this is causing a cant-find-positional-exception to be triggered
                #(let [[g' p'] (col/move-towards-by-with-grid @g % x y
                                 (g/random-double 0 10 global-rand-gen))]
                   (reset! g g')
                   p')))
      (assoc :grid @g))))



(defn -main []
  (q/defsketch Beings-Test
               :size [width height]

               :setup setup-state
               :update update-state
               :draw draw-state

               :mouse-clicked mouse-handler
               :mouse-dragged mouse-handler

               :middleware [m/fun-mode]))
