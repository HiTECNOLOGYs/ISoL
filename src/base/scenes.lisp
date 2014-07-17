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

(defclass Context ()
  ((game :initarg :game
         :accessor context-game)
   (keybindings :initarg :keybindings
                :accessor context-keybindings)))

(defun make-context (&key game keybindings)
  (make-instance 'Context
                 :game game
                 :keybindings keybindings))

(defmacro with-context ((context game-var keybindings-var) &body body)
  (with-gensyms (game keybindings)
    `(with-slots ((,game game) (,keybindings keybindings)) ,context
       (let ((,game-var ,game)
             (,keybindings-var ,keybindings))
         ,@body))))

;;; **************************************************************************
;;;  Scenes
;;; **************************************************************************

(defclass Scene ()
  ((frame :initarg :frame
          :accessor scene-frame)
   (context :initarg :context
            :accessor scene-context)
   (dispatcher :initarg :dispatcher
               :accessor scene-dispatcher))
  (:documentation "Stores information about current scene."))

(defun keybindings-list->hash-table (list)
  (let ((*keys* (make-hash-table)))
    (iter
      (for (key binding) in list)
      (after-each
        (bind-key key binding)))
    *keys*))

(defun make-scene (function &key frame game keybindings)
  (let* ((keys (etypecase keybindings
                 (list (keybindings-list->hash-table keybindings))
                 (hash-table keybindings)))
         (context (make-context :game game
                                :keybindings keys)))
    (make-instance 'Scene
                   :frame frame
                   :dispatcher function
                   :context context)))

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
      (funcall (scene-dispatcher scene)
               (scene-context scene)))))

(defgeneric game-tick (object)
  (:method ((game Game))
    (run-scene (game-current-scene game))))

;;; **************************************************************************
;;;  Dispatchers
;;; **************************************************************************

(defmacro defscene (name &body body)
  "Defines function that returns freshly created scene with all the stuff
initialized."
  (let ((handler-name (symbol-append name '/handler)))
    `(progn
       (defun ,handler-name (context)
         (with-context (context *game* *keys*)
           ,@body))
       (defun ,name (&key frame (game *game*) (keys *keys*))
         (make-scene ',handler-name
                     :frame frame
                     :game game
                     :keybindings keys)))))

(defscene game-scene ()
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

(defscene menu-scene ()
  ;; Handle menu here
  )

(defscene inventory-scene ()
  "Display interface to do all sorts of things with items PC has."
  (redraw-screen)
  (process-key (wait-for-key) *game*))
