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

;;; *********************************************************************
;;; Scenes
;;; *********************************************************************

(defclass Scene ()
  ((frame :initarg :frame
          :initform :root
          :accessor scene-frame)
   (variables :initarg :variables
              :initform nil
              :accessor scene-variables)
   (dispatcher :initarg :dispatcher
               :accessor scene-dispatcher))
  (:documentation "Stores information about current scene."))

(defun make-scene (function
                   &key (frame nil frame-given?)
                        (keybindings *keys* keybindings-given?)
                        (variables nil variables-given?))
  (let ((scene (make-instance 'Scene :dispatcher function)))
    (when frame-given?
      (setf (scene-frame scene) frame))
    (when keybindings-given?
      (let ((*keys* (make-hash-table)))
        (iter
          (for (key binding) in keybindings)
          (after-each
            (bind-key key binding)))
        (push (list '*keys* *keys*) variables)))
    (when variables-given?
      (let ((additional-variables (mapcar #'first variables))
            (additional-variables-values (mapcar #'second variables)))
        (setf (scene-variables scene)
              (list additional-variables
                    additional-variables-values))))
    scene))

(defgeneric game-current-scene (object)
  (:method ((object Game))
    (car (game-scenes object))))

(defgeneric push-scene (scene object)
  (:method ((scene Scene) (object Game))
    (push scene (game-scenes object))
    scene))

(defgeneric display-scene (scene)
  (:method ((scene Scene))
    (display (scene-frame scene))))

(defgeneric pop-scene (object)
  (:method ((object Game))
    (pop (game-scenes object))))

(defgeneric run-scene (object)
  (:method ((scene Scene))
    (when (slot-boundp scene 'dispatcher)
      (funcall (scene-dispatcher scene))))
  (:method :around ((scene Scene))
    (with-slots (keybindings variables) scene
      (progv (first variables) (second variables)
        (call-next-method)))))

(defgeneric game-tick (object)
  (:method ((game Game))
    (run-scene (game-current-scene game))))

;;; *********************************************************************
;;; Scenes dispatchers
;;; *********************************************************************

(defun game-scene ()
  "Game step. Draws map, PC, stuff and prompts player for action."
  ;; Displaying info about stuff lying on the floor.
  (with-slots (map player) *game*
    (destructuring-bind (x y) (location player)
      (let ((map-cell (map-cell-top map x y)))
        (when (typep map-cell 'Item)
          (display-message-in-minibuffer "~A is lying here." (name map-cell))))))
  (redraw-screen)
  (process-key (wait-for-key) *game*)
  (cl-tui:clear 'minibuffer))

(defun menu-scene ()
  ;; Handle menu here
  )

(defun inventory-scene ()
  (redraw-screen)
  (process-key (wait-for-key) *game*))
