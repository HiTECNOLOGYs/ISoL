;;; Copyright (C) Mark Fedurin, 2011-2014.
;;;
;;; This file is part of ISoL.
;;;
;;; ISoL is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; ISoL is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with ISoL.  If not, see <http://www.gnu.org/licenses/>.

(in-package :isol)

;;; **************************************************************************
;;;  Game class. Creating, saving, loading game state
;;; **************************************************************************

(defvar *game*)

(defclass Game ()
  ((world :initarg :world
          :initform (make-world)
          :accessor game-world)
   (scenes :initform nil
           :accessor game-scenes)
   (contexts :initform nil
             :accessor game-contexts)
   (events :initform nil
           :accessor game-events))
  (:documentation "Stores necessary info about current game."))

(defun save-game (game pathname)
  "Stores game to file."
  (cl-store:store game pathname))

(defun load-game (pathname)
  "Restores game from file and returns it."
  (cl-store:restore pathname))

(defun new-game (&rest initargs)
  (apply #'make-instance 'Game
         initargs))

;;; **************************************************************************
;;;  Contexts
;;; **************************************************************************

(defgeneric game-current-context (object)
  (:method ((object Game))
    (first (game-contexts object))))

(defgeneric push-context (context object)
  (:method ((context Context) (object game))
    (push context (game-contexts object))
    context))

(defgeneric pop-context (object)
  (:method ((object Game))
    (pop (game-contexts object))
    (game-current-context object)))

;;; **************************************************************************
;;;  Scenes
;;; **************************************************************************

(defgeneric game-current-scene (object)
  (:method ((object Game))
    (car (game-scenes object))))

(defgeneric push-scene (scene object)
  (:method ((scene Scene) (object Game))
    (push scene (game-scenes object))
    (game-current-scene object)))

(defgeneric pop-scene (object)
  (:method ((object Game))
    (pop (game-scenes object))
    (game-current-scene object)))

(defgeneric run-scene (object)
  (:method ((scene Scene))
    (when (slot-boundp scene 'dispatcher)
      (funcall (scene-dispatcher scene))))
  (:method ((game Game))
    (run-scene (game-current-scene game)))
  (:method :around ((game Game))
    (with-context ((game-current-context game))
      (call-next-method))))

(defgeneric game-tick (object)
  (:method ((game Game))
    (run-scene game)))
