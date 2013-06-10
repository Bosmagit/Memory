(ns memory.model
  (:require [noir.session :as session]))

(def empty-board [[\- \- \- \-]
                  [\- \- \- \-]
                  [\- \- \- \-]
                  [\- \- \- \-]])

(def tiles (map char(concat (range 65 73) (range 65 73))))

(def generate-board
  (vec (apply map vector(partition 4 4 (shuffle tiles)))))

(def init-state {:board empty-board :generated-board generate-board :player 1 :score [0 0] :last-lookup nil :false-lookup nil})

(defn reset-game! []
  (session/put! :game-state init-state))

(defn get-player []
  (:player (session/get :game-state)))

(defn get-score []
  (:score (session/get :game-state)))

(defn get-last-lookup []
  (:last-lookup (session/get :game-state)))

(defn get-false-lookup []
  (:false-lookup (session/get :game-state)))

(defn get-generated-board []
  (:generated-board (session/get :game-state)))

(defn turn-board-cell
  ([board point]
    (assoc-in board point (get-in (get-generated-board) point))))

(defn get-board []
  (if (= (get-last-lookup) nil)
    (:board (session/get :game-state))
    (if (= (get-false-lookup) nil)
      (turn-board-cell (:board (session/get :game-state)) (get-last-lookup))
      (turn-board-cell (turn-board-cell (:board (session/get :game-state)) (get-last-lookup)) (get-false-lookup)))))

(defn get-board-cell 
  ([row col]
    (get-board-cell (get-board) row col))
  ([board row col]
    (get-in board [row col])))

(defn turned-cell-match? [firstpoint secondpoint]
  (and
	  (= 
	    (get-in (get-generated-board) firstpoint) 
	    (get-in (get-generated-board) secondpoint))
   (not= firstpoint secondpoint)))

(defn other-player 
  ([] (other-player (get-player)))
  ([player] (if (= player 1) 2 1))) 

(defn score [player]
  (assoc (get-score) (dec player) (inc ((get-score) (dec player)))))

(defn sameCard? [card1 card2]
  (= card1 card2))

(defn winner?
  ([] false)
  ([board] false)
  ([board player] false))

(defn full-board?
  ([] (full-board? (get-board)))
  ([board] (let [all-cells (apply concat board)]
             (not-any? #(= % \-) all-cells))))

(defn new-state [row col old-state]
  (let [isSameCard (turned-cell-match? (get-last-lookup) [row col])
        isDiffirentLookup (not= [row col] (get-last-lookup))
        isNewCard (= (get-board-cell row col) \-)]
  (if (or (and isDiffirentLookup isNewCard) (not= (:false-lookup old-state) nil))
  (if (and isSameCard (not (winner? (:board old-state))) (= (:false-lookup old-state) nil))
    {:board (assoc-in (assoc-in (:board old-state) [row col] (get-board-cell (get-generated-board) row col))
                      (get-last-lookup) (get-board-cell (get-generated-board) row col))
     :generated-board (:generated-board old-state)
     :player (:player old-state)
     :score (score (:player old-state))
     :last-lookup nil
     :false-lookup nil}
    (if (= (:last-lookup old-state) nil)
          (assoc old-state :last-lookup [row col] :false-lookup nil)
          (if (= (:false-lookup old-state) nil)
            (assoc old-state :false-lookup [row col] :player (other-player (:player old-state)))
            (assoc old-state :last-lookup [row col] :false-lookup nil))))
  old-state)))

(defn play! [row col]
  (session/swap! (fn [session-map]
                   (assoc session-map :game-state 
                          (new-state row col (:game-state session-map))))))