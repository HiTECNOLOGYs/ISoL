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
;;;  Contexts
;;; **************************************************************************

(defvar *context*)

(defclass Context ()
  ((game :initarg :game
         :accessor context-game)
   (input-mode :initarg :input-mode
               :accessor context-input-mode)
   (variables :initform (make-hash-table)
              :documentation "This hash-table is used to save data between
scene dispatcher iteration. Scene dispatcher can't go into infinite loop
because scene might need switching while it does and there's not other
obvious way to save necessary data until dispatcher is called again."
              :accessor context-variables)))

(defun context-var (var)
  "Returns value of contextual variables."
  (gethash var (context-variables *context*)))

(defun (setf context-var) (new-value var)
  "SETF-function for CONTEXT-VAR."
  (setf (gethash var (context-variables *context*)) new-value))

(defun make-context (&key (game *game*) (input-mode *input-mode*))
  (make-instance 'Context
                 :game game
                 :input-mode input-mode))

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

(defmacro with-context (context &body body)
  "Binds game and key bindsings to given symbols, sets current context
(by binding it to *CONTEXT*)."
  `(let ((*context* ,context))
     ,@body))

;;; **************************************************************************
;;;  Scenes
;;; **************************************************************************

(defclass Scene ()
  ((frame :initarg :frame
          :accessor scene-frame)
   (dispatcher :initarg :dispatcher
               :accessor scene-dispatcher))
  (:documentation "Stores information about current scene."))

(defun make-scene (function &key frame)
  (make-instance 'Scene
                 :frame frame
                 :dispatcher function))

(defgeneric game-current-scene (object)
  (:method ((object Game))
    (car (game-scenes object))))

(defgeneric push-scene (scene object)
  (:method ((scene Scene) (object Game))
    (push scene (game-scenes object))
    (game-current-scene object)))

(defgeneric display-scene (scene)
  (:method ((scene Scene))
    (display (scene-frame scene))))

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
    (with-context (game-current-context game)
      (call-next-method))))

(defgeneric game-tick (object)
  (:method ((game Game))
    (run-scene game)))

;;; **************************************************************************
;;;  Dispatchers
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

(defscene game-scene
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

(defscene menu-scene
  ;; Handle menu here
  )

(defscene inventory-scene
  "Display interface to do all sorts of things with items PC has."
  (redraw-screen)
  (process-key (wait-for-key) *game*))
