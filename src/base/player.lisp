(in-package :isol)

(defclass Player (Creature)
  ((hp :initform 100)
   (wp :initform 100))
  (:documentation "Player character."))

(defun player-x (player)
  (first (location player)))

(defun (setf player-x) (new-value player)
  (setf (first (location player)) new-value))

(defun player-y (player)
  (second (location player)))

(defun (setf player-y) (new-value player)
  (setf (second (location player)) new-value))

(defmethod move-creature ((player Player) map x y)
  "Moves player relative and checks map cell to passability."
  (when (map-cell-passable-p map (+ (player-x player) x) (+ (player-y player) y))
    (incf (player-x player) x)
    (incf (player-y player) y)
    (list (player-x player) (player-y player))))

(defmethod pick-up-object ((player Player) map)
  (destructuring-bind (player-x player-y) (location player)
    (when (takable-p (get-map-cell-top map player-x player-y))
      (push (cons (length (inventory player))
                  (pop-object map player-x player-y))
            (inventory player)))))

(defmethod print-object ((player Player) (stream (eql :game-window)))
  (destructuring-bind (player-x player-y) (location player)
    (draw-char-at :game-window (display-character player) player-x player-y)))

(defmethod print-object ((player Player) (stream (eql :info-window)))
  (mapc (curry #'wprintw-newline :info-window)
        (append (list (format nil "HP: ~D" (hp player))
                      (format nil "WP: ~D" (wp player))
                      ""
                      "Inventory:"
                      "---")
                (mapcar #'(lambda (item)
                            (format nil "[~D] ~A"
                                    (car item)
                                    (name (cdr item))))
                        (inventory player)))))

(define-key-processor #\j ()
  (move-creature player map 0 1))

(define-key-processor #\k ()
  (move-creature player map 0 -1))

(define-key-processor #\h ()
  (move-creature player map -1 0))

(define-key-processor #\l ()
  (move-creature player map 1 0))

(define-key-binding #\; #'pick-up-object)
