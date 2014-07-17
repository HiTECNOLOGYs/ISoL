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

(defun take-a-nap ()
  (loop (sleep most-positive-fixnum)))

(defun first-n (n list)
  (unless (zerop n)
    (cons (car list)
          (first-n (1- n) (cdr list)))))

(defun ensure-string-within-length (n string &optional (replacement "$"))
  (if (<= (length string) n)
    string
    (concatenate 'string
                 (subseq string 0 (- n (length replacement)))
                 replacement)))

(defun 2d-array->list (array &key transformer (key #'aref) reverse-axes?)
  "Converts 2 dimensional array to list of lists whcih are rows of array."
  (destructuring-bind (size-y size-x) (array-dimensions array)
    (loop for i below size-y
          collect (loop
                    for j below size-x
                    for cell = (if reverse-axes?
                                 (funcall key array j i)
                                 (funcall key array i j))
                    collect (if (null transformer)
                              cell
                              (funcall transformer cell))))))

(defmacro doarray ((x-var y-var array) &body body)
  "Iterates through `array' from top left angle to down right angle.
Binds `x-var' to current x position and `y-var' to current y position."
  `(destructuring-bind (x-size y-size) (array-dimensions ,array)
     (dotimes (,y-var y-size)
       (dotimes (,x-var x-size)
         ,@body))))

(defun symbol-append (symbol-1 symbol-2 &optional (package *package*))
  (intern (concatenate 'string
                       (symbol-name symbol-1)
                       (symbol-name symbol-2))
          package))
