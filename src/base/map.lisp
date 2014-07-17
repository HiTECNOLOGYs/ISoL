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
;;;  Rendering map
;;; **************************************************************************

(defun render-map (map)
  "Transforms map in human-readable and printable form."
  (2d-array->list map :transformer #'display-character
                      :key #'map-cell-top))

;;; **************************************************************************
;;;  Various map manipulation stuff
;;; **************************************************************************

(defun map-cell-value (map x y)
  "Returns value of cell of `map' with coordinates (`x';`y')."
  (aref map y x))

(defun (setf map-cell-value) (new-value map x y)
  "SETF-function for MAP-CELL-VALUE."
  (setf (aref map y x) new-value))

(defun map-cell-passable-p (map x y)
  "Checks if some map place is passable for player."
  (every #'passable-p (map-cell-value map x y)))

(defun push-object (map x y object)
  "Pushes new object on top of stack of others."
  (push object (map-cell-value map x y))
  object)

(defun pop-object (map x y)
  "Removes object from pile of objects."
  (pop (map-cell-value map x y)))

(defun map-cell-top (map x y)
  "Returns objects from the top of the pile."
  (first (map-cell-value map x y)))
