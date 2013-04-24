(in-package #:isol)

(defclass Player (Creature) ()
  (:documentation "Player character."))

(defmethod move-creature ((player Player) map x y)
  "Moves player relative and checks map cell to passability."
  (destructuring-bind (player-x player-y) (location player)
    (when (map-cell-passable-p map (+ player-x x) (+ player-y y))
      (incf player-x x)
      (incf player-y y)
      (list player-x player-y))))
