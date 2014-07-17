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
;;;  Objects generation
;;; **************************************************************************

(defvar *objects-templates* (make-hash-table))

(defun object-generator (object-id)
  "Returns anonymous function that creates object with given ID when called."
  (gethash object-id *objects-templates*))

(defun (setf object-generator) (new-value object-id)
  "SETF-function for OBJECT-GENERATOR."
  (setf (gethash object-id *objects-templates*) new-value))

(defun parse-object-generation-rules (class rules)
  "Translates object generation rules to actual code that generates desired object."
  ;; At the moment rules are treated as initargs for class.
  `((apply #'make-instance ',class
           ,@rules
           parameters)))

(defmacro define-object-generator (object-id class &body body)
  "Binds some symbol to lambda which will create instances of object of given
class by given generation rules."
  `(setf (object-generator ',object-id)
         #'(lambda (&rest parameters)
             (declare (ignorable parameters))
             ,@(parse-object-generation-rules class body))))

(defun generate-object (id &rest parameters)
  "Returns instace of object for given `ID'"
  (awhen (object-generator id)
    (apply it parameters)))

(define-object-generator Wall Map-Element
  :name "Wall"
  :description "Just rusty old stone wall."
  :display-character #\#
  :hp 10000
  :material 'stone)

(define-object-generator Ground Map-Element
  :name "Ground"
  :passable? t
  :description "Nothing in here."
  :display-character #\.
  :hp :100000
  :material 'stone)

(define-object-generator Gun Weapon
  :name "Revolver"
  :description "A bit rusty and dirty old revolver with no ammo."
  :damage-value 15
  :kind (list :gun :bullets-9mm)
  :size 2
  :weight 150)

(define-object-generator Rock Map-Object
  :name "Rock"
  :description "A hunge gray rock lying on the floor."
  :movable? t
  :passable? t
  :display-character #\*
  :hp :1000
  :material 'stone)

(define-object-generator Knife Weapon
  :name "Knife"
  :description "Ordinary steel kitchen knife."
  :damage-value 3
  :kind (list :melee)
  :size 1
  :weight 50)

;;; **************************************************************************
;;;  Map generation
;;; **************************************************************************

;; ----------------
;; High level generation function

(defun gen-new-map (type)
  (ecase type
    (:testing (gen-testing-map))
    (:empty (gen-empty-map))))

(defun gen-empty-map ()
  (let ((map (make-array (list 30 30)
                         :initial-element nil)))
    (set-map-borders map)))

(defun gen-testing-map ()
  ;;; Generating sample map here for debugging purposes
  (let ((map (gen-empty-map)))
    (push-object map 2 2 (generate-object 'Gun))
    map))

;; ----------------
;; Primitives
;;
;; All the primitives are destructive and operate on exiting map for the sake of
;; simplicity. I know I'll have problems when I decide to go multi-core but I'm pretty
;; confident I can take care of it.

(defun set-map-borders (map)
  "Puts walls at the edges of the map."
  (destructuring-bind (y x) (array-dimensions map)
    (iter
      (for i from 0 to (1- y))
      (after-each
        (push-object map 0 i      (generate-object 'Wall))
        (push-object map (1- x) i (generate-object 'Wall))))
    (iter
      (for i from 1 to (- x 2))
      (after-each
        (push-object map i 0      (generate-object 'Wall))
        (push-object map i (1- y) (generate-object 'Wall)))))
  map)
