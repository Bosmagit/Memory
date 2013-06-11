(ns memory.test.model
  (:use memory.model)
  (:use clojure.test)
  (:require [memory.test.testdata :as td]))

(defn count-appearances-of-char [board x]
  (count (filter #(= x % ) (flatten board))))

(deftest generate-board-test
  (let [board generate-board]
  (doseq [x tiles]
    (is (= 2 (count-appearances-of-char board x))))))

(deftest get-board-cell-test
  (let [testboard [[\X \- \-]
                   [\- \O \-]
                   [\- \- \X]]]
    (is (get-board-cell testboard 0 0) \X)
    (is (get-board-cell testboard 0 1) \-)
    (is (get-board-cell testboard 1 1) \O)
    (is (get-board-cell testboard 2 2) \X)))

(deftest full-board?-test
  (doseq [player [\X \O]]
    (doseq [board (td/full-boards player)]
      (is (= (full-board? board) true)
          (str "Board should be considered full, but isn't: " board)))
    (doseq [board (td/no-full-boards player)]
      (is (= (full-board? board) false)
          (str "Board should not be considered full, but is: " board)))))