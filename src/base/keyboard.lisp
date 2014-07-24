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
;;;  Modes
;;; **************************************************************************

(defvar *input-mode* :normal)
(defvar *modes* (make-hash-table))
(defvar *keys*)

(defun mode-keys (mode)
  (gethash mode *modes*))

(defun (setf mode-keys) (new-value mode)
  (setf (gethash mode *modes*) new-value))

(defun key-binding (key)
  "Returns function binded to given key."
  (if (boundp '*keys*)
    (gethash key *keys*)
    (gethash key (mode-keys *input-mode*))))

(defun (setf key-binding) (function key)
  (if (boundp '*keys*)
    (setf (gethash (char-code key) *keys*)                   function)
    (setf (gethash (char-code key) (mode-keys *input-mode*)) function)))

(defun bind-key (key function)
  "Binds given character to some function."
  (setf (key-binding key) function))

(defun clear-key-bindings ()
  "Removes all key bindings."
  (if (boundp '*keys*)
    (clrhash *keys*)
    (clrhash (mode-keys *input-mode*))))

(defun process-key (key &rest arguments)
  "Runs function binded to some `key' with given arguments."
  (awhen (key-binding (etypecase key
                        (integer key)
                        (character (char-code key))
                        (symbol key)))
    (apply it arguments)))

(defun wait-for-key ()
  "Waits until user presses key and returns it."
  (cl-tui:read-key))

;;; **************************************************************************
;;;  Macros
;;; **************************************************************************

(defmacro with-input-mode (mode-name &body body)
  "Executes `body' with specific input mode enabled."
  `(let ((*input-mode* ,mode-name)
         (*keys* (mode-keys ,mode-name)))
     ,@body))

(defmacro define-input-mode (name &body bindings)
  "Adds new input mode and binds keys for it."
  `(progn
     (setf (mode-keys ,name) (make-hash-table :test 'equal))
     (iter
       (for (key binding) in ',bindings)
       (after-each
         (bind-key key binding)))))

(defmacro define-key-binding (key mode-name function)
  `(with-input-mode ,mode-name
     (bind-key ,key ',function)))

(defmacro define-key-handler (name (key mode-name) &body body)
  (let ((name (symbol-append 'key/ name)))
    `(progn
       (defun ,name (game)
         (declare (ignorable game))
         ,@body)
       (define-key-binding ,key ,mode-name ,name))))
