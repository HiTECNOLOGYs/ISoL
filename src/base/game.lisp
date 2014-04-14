(in-package :isol)

(define-constant +info-window-size+ 40
  :test #'=)
(define-constant +minibuffer-size+ 2
  :test #'=)
(defparameter *screen-size* nil)

(defclass Game ()
  ((map :initarg :map
        :accessor game-map)
   (player :initarg :player
           :initform (make-instance 'player :location (list 1 1))
           :accessor game-player)
   (creatures :initarg :creatures
              :initform (make-hash-table)
              :accessor game-creatures)
   (scenes :initform nil
           :accessor game-scenes))
  (:documentation "Stores necessary info about current game."))

(defun make-game (&rest initargs)
  (apply #'make-instance 'Game
         initargs))
