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
;;;  High level
;;; **************************************************************************

(defun gen-new-map (type)
  (ecase type
    (:testing (gen-testing-map))
    (:empty (gen-empty-map))))

(defun gen-empty-map ()
  (let ((map (make-array (list 10 10)
                         :initial-element nil)))
    (doarray* (i cell map)
      (push (generate-object 'Ground) cell))
    (set-map-borders map)))

(defun gen-testing-map ()
  ;;; Generating sample map here for debugging purposes
  (let ((map (gen-empty-map)))
    (push-object map 2 1 (generate-object 'Gun))
    (push-object map 3 1 (generate-object 'Knife))
    (push-object map 4 1 (generate-object 'Knife))
    (generate-map-primitive/small-room map 2 2 :top 1)
    map))

;;; **************************************************************************
;;;  Debug
;;; **************************************************************************

(defun set-map-borders (map)
  "Puts walls at the edges of the map."
  (destructuring-bind (y x) (array-dimensions map)
    (generate-map-primitive/borders map 0 0 x y))
  map)

;;; **************************************************************************
;;;  Primitives
;;; **************************************************************************
;;;
;;; All the primitives are destructive and operate on exiting map for the sake of
;;; simplicity. I know I'll have problems when I decide to go multi-core but I'm pretty
;;; confident I can take care of it.

;; ----------------
;; Base

(defvar *map-primitives* (make-hash-table))

(defun map-primitive (name)
  (gethash name *map-primitives*))

(defun (setf map-primitive) (new-value name)
  (setf (gethash name *map-primitives*) new-value))

(defun generate-map-primitive (map primitive-name &rest parameters)
  (awhen (map-primitive primitive-name)
    (apply it map parameters)))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun parse-map-primitive-definition (definition)
    "Transforms map primitive definition into code that creates one."
    (let+ (((x-size y-size) (cdr (assoc :size definition)))
           (fill-object (cdr (assoc :fill definition)))
           (subcomponents (cdr (assoc :components definition)))
           (parameters (cdr (assoc :parameters definition)))
           (body (cdr (assoc :gen definition)))
           (documentation (car (cdr (assoc :doc definition))))
           (final-arguments-list
             `(map x y ; Obligatory parameters
                   ,@(when (symbolp x-size)
                       `(,x-size))
                   ,@(when (symbolp y-size)
                       `(,y-size))
                   ,@(when (symbolp fill-object)
                       fill-object)
                   ,@parameters)))
      (values
        `(lambda ,final-arguments-list
           (let* ((x-size ,x-size)
                  (y-size ,y-size)
                  (x1 x)
                  (y1 y)
                  (x2 (+ x ,(if (symbolp x-size)
                              '(1- x-size)
                              (1- x-size))))
                  (y2 (+ y ,(if (symbolp y-size)
                              '(1- y-size)
                              (1- y-size)))))
             (declare (ignorable x-size y-size x1 y1 x2 y2))
             ,@(iter
                 (for (name . params) in subcomponents)
                 (collecting `(generate-map-primitive map ',name ,@params)))
             ,@(when fill-object
                 `((iter
                     (for i from y1 to y2)
                     (after-each
                       (iter
                         (for j from x1 to x2)
                         (after-each
                           (push-object map j i (generate-object ,fill-object))))))))
             ,@body)
           map)
        documentation
        final-arguments-list))))

(defmacro define-map-primitive (name &body body)
  (multiple-value-bind (function doc args) (parse-map-primitive-definition body)
    `(progn
       (setf (map-primitive ',name)
             #',function)
       (defun ,(intern (format nil "GENERATE-MAP-PRIMITIVE/~A" name)) ,args
         ,doc
         (generate-map-primitive map ',name ,@(remove 'map args))))))

;; ----------------
;; Primitives

(define-map-primitive Borders
  (:size x-size y-size)
  (:doc "Limites perimeter of given area by walls.")
  (:gen
    (iter
      (for i from y1 to y2)
      (after-each
        (push-object map x1 i (generate-object 'Wall))
        (push-object map x2 i (generate-object 'Wall))))
    (iter
      (for i from (1+ x1) to (1- x2))
      (after-each
        (push-object map i y1 (generate-object 'Wall))
        (push-object map i y2 (generate-object 'Wall))))))

(define-map-primitive Exit-door
  (:parameters door-side door-position)
  (:size x-size y-size)
  (:doc "Sets door on the border of given area.")
  (:gen
    (let ((door (generate-object 'Wooden-Door)))
      (ecase door-side
        (:right  (replace-top-object map x2 (+ y1 door-position) door))
        (:left   (replace-top-object map x1 (+ y1 door-position) door))
        (:top    (replace-top-object map (+ x1 door-position) y1 door))
        (:bottom (replace-top-object map (+ y1 door-position) y2 door))))))

(define-map-primitive Small-room
  (:parameters door-side door-position)
  (:size 5 5)
  (:doc "Generates box of walls and sets door on appropriate space.")
  (:components
    (Borders x1 y1 x-size y-size)
    (Exit-door x1 y1 x-size y-size door-side door-position))
  (:gen
    (when (or (<= x-size door-position)
              (<= y-size door-position))
      (error "Hey! Door position is bigger than the room!"))))
