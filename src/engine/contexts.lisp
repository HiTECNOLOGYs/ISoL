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
;;;  Contexts
;;; **************************************************************************

(defvar *context*)

(defclass Context ()
  ((variables :initform (make-hash-table)
              :documentation "This hash-table is used to save data between
scene dispatcher iteration. Scene dispatcher can't go into infinite loop
because scene might need switching while it does and there's not other
obvious way to save necessary data until dispatcher is called again."
              :accessor context-variables)))

(defun context-var (var &optional default)
  "Returns value of contextual variables."
  (gethash var (context-variables *context*) default))

(defun (setf context-var) (new-value var)
  "SETF-function for CONTEXT-VAR."
  (setf (gethash var (context-variables *context*)) new-value))

(defmacro with-context ((context) &body body)
  "Binds game and key bindsings to given symbols, sets current context (by
binding it to *CONTEXT*)."
  `(let ((*context* ,context))
     ,@body))

(defmacro do-context-vars ((name value context) &body body)
  "Iterates though context variables."
  `(maphash #'(lambda (,name ,value)
                ,@body)
            (context-variables ,context)))

(defun make-context (&rest variables)
  "Returns freshly made context with given  variables initialized."
  (let ((context (make-instance 'Context)))
    (with-context (context)
      (loop
        for (variable value) on variables by #'cddr
        doing (setf (context-var variable) value)))
    context))

(defun copy-context (context &rest overwrites)
  "Makes fresh context, copies variables from old one, replaces contents of
some with overwrites, adds some new."
  (let ((new-context (make-context)))
    (with-context (new-context)
      (do-context-vars (var value context)
        (setf (context-var var) value))
      (loop
        for (var value) on overwrites by #'cddr
        doing (setf (context-var var) value)))
    new-context))
