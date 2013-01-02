(in-package #:isol.player)

(defstruct Player
  (position '(0 0))
  (character #\@))

(declaim (inline player-x))
(defun player-x (player)
  "Short binding for getting player X position."
  (first (player-position player)))

(declaim (inline player-y))
(defun player-y (player)
  "Short binding for getting player Y position."
  (second (player-position player)))

(defun move-player (player map x y)
  "Moves player relative and checks map cell to passability."
  (destructuring-bind (player-x player-y) (player-position player)
      (when (check-passability map (+ player-x x) (+ player-y y))
        (incf player-x x)
        (incf player-y y))))
