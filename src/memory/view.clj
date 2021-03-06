(ns memory.view
  (:use hiccup.form
        [hiccup.def :only [defhtml]]
        [hiccup.element :only [link-to]]
        [hiccup.page :only [html5 include-css]])
  (:require [memory.model :as model]))

(defhtml layout [& content]
  (html5
   [:head
    [:title "Welcome to memory-luminus"]
    (include-css "/css/memory.css")]
   [:body [:div#wrapper content]]))

(defn cell-html [rownum colnum cell with-submit?] 
  [:td 
   [:input {:name (str "b" rownum colnum) 
            :value (str cell)
            :type (if with-submit? 
                    "submit" 
                    "button")}]])
  
(defn row-html [rownum row with-submit?]
  [:tr (map-indexed (fn [colnum cell]
                      (cell-html rownum colnum cell with-submit?))
                    row)])
  
(defn board-html [board with-submit?]
  (form-to [:post "/"]
           [:table 
            (map-indexed (fn [rownum row]
                           (row-html rownum row with-submit?)) 
                         board)]))

(defn play-screen []
  (layout
    [:div 
     [:p "Player " (model/get-player) ", it is your turn!"]
     (board-html (model/get-board) true)]
    [:div
     [:p "Score player 1: " ((model/get-score) 0)]
     [:p "Score player 2: " ((model/get-score) 1)]
    ]))

(defn winner-screen [winner]
  (layout
    [:div 
   [:p "The winner is: " winner]
   (board-html (model/get-board) false)
   [:p "Score player 1: " ((model/get-score) 0)]
   [:p "Score player 2: " ((model/get-score) 1)]
   (link-to "/" "Reset")]))

(defn draw-screen []
  (layout
    [:div
     [:p "It's a draw!"]
     (board-html (model/get-board) false)
     [:p "Score player 1: " ((model/get-score) 0)]
     [:p "Score player 2: " ((model/get-score) 1)]
     (link-to "/" "Reset")]))
  
