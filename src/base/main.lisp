(in-package :isol)

(defparameter *game* (make-game)
  "Stores current game instance.")

(defun main ()
  "Start point."
  (setf *game* (make-game))
  (run-game *game*))
