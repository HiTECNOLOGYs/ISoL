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
;;;  Keyboard
;;; **************************************************************************

;; ----------------
;; Modes

(defvar *keys* (make-hash-table))

(defun mode-keys (mode)
  (gethash mode *keys*))

(defun (setf mode-keys) (new-value mode)
  (setf (gethash mode *keys*) new-value))

(defun key-binding (mode key modifiers)
  "Returns function binded to given key under given mode."
  (gethash (list* (code-char key) modifiers) (mode-keys mode)))

(defun (setf key-binding) (function mode key modifiers)
  (setf (gethash (list* key modifiers) (mode-keys mode)) function))

(defun bind-key (mode key modifiers function)
  "Binds given character to some function."
  (setf (key-binding mode key modifiers) function))

(defun clear-key-bindings (mode)
  "Removes all key bindings."
  (clrhash (mode-keys mode))
  (remhash mode *keys*)
  (values))

(defun handle-key (key modifiers &rest arguments)
  "Runs function binded to some `key' with given arguments."
  (awhen (key-binding (context-var :input-mode :normal)
                      (etypecase key
                        (integer key)
                        (character (char-code key))
                        (symbol key))
                      modifiers)
    (apply it arguments)))

;; ----------------
;; Macros

(defmacro with-input-mode (mode-name &body body)
  "Executes `body' with specific input mode enabled."
  (with-gensyms (context-gensym)
    `(let ((,context-gensym (copy-context *context* :input-mode ,mode-name)))
       (with-context (,context-gensym)
         ,@body))))

(defmacro define-input-mode (name &body bindings)
  "Adds new input mode and binds keys for it."
  `(progn
     (setf (mode-keys ,name) (make-hash-table :test 'equal))
     ,@(iter
         (for (key modifiers function) in bindings)
         (collecting
           (bind-key name key modifiers function)))))

(defmacro define-key-binding (mode key modifiers function)
  `(bind-key ,mode ,key ',modifiers ',function))

(defmacro define-key-handler (name (mode key modifiers) &body body)
  (let ((name (symbol-append 'key/ name)))
    `(progn
       (defun ,name (game)
         (declare (ignorable game))
         ,@body)
       (define-key-binding ,mode ,key ,modifiers ,name))))

;; ----------------
;; SDL macros

(defmacro define-window-keyboard-handler (name &body body)
  `(defmethod sdl2.kit:keyboard-event ((window ,name) state timestamp repeat-p keysym)
     ,@body))

;;; **************************************************************************
;;;  Mouse
;;; **************************************************************************

(defvar *screen-regions* (make-hash-table :test 'equal))

;; ----------------
;; Screen regions handling

(defun screen-region-binding (x1 y1 x2 y2)
  (gethash (list x1 y1 x2 y2) *screen-regions*))

(defun (setf screen-region-binding) (function x1 y1 x2 y2)
  (setf (gethash (list x1 y1 x2 y2) *screen-regions*) function))

(defun bind-screen-region (x1 y1 x2 y2 function)
  (setf (screen-region-binding x1 y1 x2 y2) function))

;; ----------------
;; Macros

(defmacro define-window-region-binding ((x1 y1 x2 y2) function)
  `(bind-screen-region ,x1 ,y1 ,x2 ,y2 ,function))

;; ----------------
;; SDL macros

(defmacro define-window-mouse-button-handler (name &body body)
  `(defmethod sdl2.kit:mousebutton-event ((window ,name) state timestamp button x y)
     ,@body))

(defmacro define-window-mouse-motion-handler (name &body body)
  `(defmethod sdl2.kit:mousemotion-event ((window ,name) timestamp x y)
     ,@body))
