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
           :accessor game-scenes)
   (log :initform nil
        :accessor game-log))
  (:documentation "Stores necessary info about current game."))

(defun make-game (&rest initargs)
  (apply #'make-instance 'Game
         initargs))
