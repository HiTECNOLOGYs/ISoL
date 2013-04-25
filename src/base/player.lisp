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

(define-key-processor #\j ()
  (move-creature player map 0 1))

(define-key-processor #\k ()
  (move-creature player map 0 -1))

(define-key-processor #\h ()
  (move-creature player map -1 0))

(define-key-processor #\l ()
  (move-creature player map 1 0))
