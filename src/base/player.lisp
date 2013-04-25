(in-package #:isol)

(defclass Player (Creature) ()
  (:documentation "Player character."))

(defmethod move-creature ((player Player) map x y)
  "Moves player relative and checks map cell to passability."
  (destructuring-bind (player-x player-y) (location player)
    (when (map-cell-passable-p map (+ player-x x) (+ player-y y))
      (incf (first (location player)) x)
      (incf (second (location player)) y)
      (list (first (location player)) (second (location player))))))
