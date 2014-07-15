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

(defparameter *objects-map-reader-symbols* nil)

(defmacro define-object-map-symbol (symbol class &body default-initargs)
  "Binds some symbol to lambda which will create instances of this object with
some values preinitialized when called."
  `(pushnew (cons ,symbol
                  #'(lambda (&rest initargs)
                      (if initargs
                          (apply #'make-instance ',class
                                 ,@default-initargs initargs)
                          (make-instance ',class ,@default-initargs))))
            *objects-map-reader-symbols*
            :test #'eql
            :key #'car))

(defun get-object-instance-from-symbol (symbol &rest initargs)
  "Returns instace of object for given `symbol'"
  (when-let (function (cdr (assoc symbol *objects-map-reader-symbols*)))
    (apply function initargs)))

(define-object-map-symbol :Wall Map-Element
  :name "Wall"
  :description "Just rusty old stone wall."
  :display-character #\#
  :hp 10000
  :material 'stone)

(define-object-map-symbol :Ground Map-Element
  :name "Ground"
  :passable? t
  :description "Nothing in here."
  :display-character #\.
  :hp :100000
  :material 'stone)

(define-object-map-symbol :Gun Weapon
  :name "Revolver"
  :description "A bit rusty and dirty old revolver with no ammo."
  :damage-value 15
  :kind (list :gun :bullets-9mm)
  :size 2
  :weight 150)

(define-object-map-symbol :Rock Map-Object
  :name "Rock"
  :description "A hunge gray rock lying on the floor."
  :movable? t
  :passable? t
  :display-character #\*
  :hp :1000
  :material 'stone)

(define-object-map-symbol :Knife Weapon
  :name "Knife"
  :description "Ordinary steel kitchen knife."
  :damage-value 3
  :kind (list :melee)
  :size 1
  :weight 50)

(define-object-map-symbol :Long-Name Cloth
  :name "Ahahahyapogarelpohardkoru"
  :description "...")

;;; **************************************************************************
;;;  Map generation
;;; **************************************************************************

(defun gen-new-map (type)
  (ecase type
    (:testing
      (gen-testing-map))))

(defun gen-testing-map ()
  ;;; Generating sample map here for debugging purposes
  (make-array (list 10 10)
              :initial-element nil))