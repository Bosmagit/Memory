(ns memory.model
  (:require [noir.session :as session]))

(def empty-board [[\- \- \- \-]
                  [\- \- \- \-]
                  [\- \- \- \-]
                  [\- \- \- \-]])

(def filled-board [[\A \A \B \B]
                  [\C \C \D \D]
                  [\E \E \F \F]
                  [\G \G \H \H]])

(def init-state {:board empty-board :player 1 :score [0 0] :last-lookup nil})

(defn reset-game! []
  (session/put! :game-state init-state))

(defn get-player []
  (:player (session/get :game-state)))

(defn get-score []
  (:score (session/get :game-state)))

(defn get-last-lookup []
  (:last-lookup (session/get :game-state)))

(defn turn-board-cell
  ([board point]
    (assoc-in board point (get-in filled-board point))))

(defn get-board []
  (if (= (get-last-lookup) nil)
    (:board (session/get :game-state))
    (turn-board-cell (:board (session/get :game-state)) (get-last-lookup))))

(defn get-board-cell 
  ([row col]
    (get-board-cell (get-board) row col))
  ([board row col]
    (get-in board [row col])))

(defn turned-cell-match? [firstpoint secondpoint]
  (and
	  (= 
	    (get-in filled-board firstpoint) 
	    (get-in filled-board secondpoint))
   (not= firstpoint secondpoint)))

(defn other-player 
  ([] (other-player (get-player)))
  ([player] (if (= player 1) 2 1))) 

(defn score [player]
  (assoc (get-score) (dec player) (inc ((get-score) (dec player)))))

(defn sameCard? [card1 card2]
  (= card1 card2))

(defn winner-in-rows? [board player]
  (boolean (some (fn [row] (every? (fn [c] (= c player)) row)) board)))

(defn transposed-board [board]
  (vec (apply map vector board)))

(defn winner-in-cols? [board player]
  (winner-in-rows? (transposed-board board) player))

(defn winner-in-diagonals? [board player]
  (let [diag-coords [[[0 0] [1 1] [2 2]]
                     [[0 2] [1 1] [2 0]]]]
    (boolean (some (fn [coords] 
                     (every? (fn [coord] 
                               (= player (apply get-board-cell board coord))) 
                             coords))
                   diag-coords))))

(defn winner?
  ([] false)
  ([board] false)
  ([board player] false))

(defn winnerold?
  "checks if there is a winner. when called with no args, checks for player X and player O.
returns the character for the winning player, nil if there is no winner"
  ([] (winner? (get-board)))
  ([board]
    (boolean (or (winner? board \X)
                 (winner? board \O))))
  ([board player]
    (if (or (winner-in-rows? board player)
            (winner-in-cols? board player)
            (winner-in-diagonals? board player))
      player)))

(defn full-board?
  ([] (full-board? (get-board)))
  ([board] (let [all-cells (apply concat board)]
             (not-any? #(= % \-) all-cells))))

(defn new-state [row col old-state]
  (let [newCard (get-board-cell filled-board row col)
        isSameCard (turned-cell-match? (get-last-lookup) [row col])]
  (if (and isSameCard (not (winner? (:board old-state))))
    {:board (assoc-in (assoc-in (:board old-state) [row col] (get-board-cell filled-board row col))
                      (get-last-lookup) (get-board-cell filled-board row col))
     :player (:player old-state)
     :score (score (:player old-state))
     :last-lookup nil }
    (if (= (:last-lookup old-state) nil)
          (assoc old-state :last-lookup [row col])
          (assoc old-state :last-lookup nil :player (other-player (:player old-state)))))))

(defn play! [row col]
  (session/swap! (fn [session-map]
                   (assoc session-map :game-state 
                          (new-state row col (:game-state session-map))))))