;;; Copyright (C) Mark Fedurin, 2011-2015.
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
;;;  Scenes
;;; **************************************************************************

(defclass Scene ()
  ((dispatcher :initarg :dispatcher
               :accessor scene-dispatcher))
  (:documentation "Stores information about current scene."))

(defun make-scene (function)
  (make-instance 'Scene
                 :dispatcher function))

;;; **************************************************************************
;;;  Macros
;;; **************************************************************************

(defmacro defscene (name &body body)
  "Defines function that returns freshly created scene with all the stuff
initialized."
  (let ((handler-name (symbol-append name '/handler)))
    `(progn
       (defun ,handler-name ()
         (let ((*game* (context-game *context*)))
           (with-input-mode (context-input-mode *context*)
             ,@body)))
       (defun ,name (&key frame (game *game*) (input-mode *input-mode*))
         (values (make-scene ',handler-name
                             :frame frame)
                 (make-context :game game
                               :input-mode input-mode))))))
