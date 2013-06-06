(ns memory.controller
  (:use compojure.core)
  (:require [compojure.core :as compojure]
            [memory.view :as view]
            [memory.model :as model]))

(defn start-page []
  (model/reset-game!)
  (view/play-screen))

(defn turn-page [button-pressed]
  (let [button-id (name (first (keys button-pressed)))
        rownr (Integer/parseInt (str (second button-id)))
        colnr (Integer/parseInt (str (nth button-id 2)))]
    (model/play! rownr colnr)
    (if-let [winner (model/winner?)]
      (view/winner-screen winner)
      (if (model/full-board?)
        (view/draw-screen)
        (view/play-screen)))))

(defroutes memory-routes
  (GET "/" [] (start-page))
  (POST "/" {button-pressed :params} (turn-page button-pressed)))
