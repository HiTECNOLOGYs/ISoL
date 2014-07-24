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
;;;  Game object. Creating, saving, loading game state
;;; **************************************************************************

(defvar *game*)

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
   (contexts :initform nil
             :accessor game-contexts)
   (log :initform nil
        :accessor game-log)
   (messages-buffer :initform nil
                    :accessor game-messages-buffer))
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
;;;  Messages
;;; **************************************************************************

(defun log-game-message (game format-string &rest format-args)
  (push (apply #'format nil format-string format-args)
        (game-log game)))

(defun display-message (game format-string &rest format-args)
  (let ((string (apply #'format nil format-string format-args)))
    (push string (game-messages-buffer game))
    string))

(defun flush-messages-to-minibuffer (game)
  (let ((minibuffer-width (second (cl-tui:frame-size (cl-tui:frame 'minibuffer)))))
    (with-slots (messages-buffer) game
      (dolist (message messages-buffer (values))
        (cl-tui:clear 'minibuffer)
        (put-text 'minibuffer 0 0
                  (ensure-string-within-length
                    minibuffer-width message
                    :replacement "[Press space]"
                    :always-include-replacement? (> (length messages-buffer) 1)))
        (redraw-screen)
        (when (> (length messages-buffer) 1)
          (wait-confirmation))
        (log-game-message game (pop messages-buffer))))))
