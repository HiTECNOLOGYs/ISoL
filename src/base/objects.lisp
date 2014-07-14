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

(defclass Object ()
  ((display-character :initarg :display-character
                      :accessor display-character
                      :initform #\0)
   (name :initarg :name
         :accessor name)
   (description :initarg :description
                :accessor description)
   (location :initarg :location
             :initform (list 0 0)
             :accessor location)
   (takable? :initarg :takable?
             :accessor takable-p)
   (passable? :initarg :passable?
              :accessor passable-p)
   (hp :initarg :hp
       :accessor hp)
   (material :initarg :material
             :accessor material
             :documentation "Needs to compute damage to object when someone tries to destroy it"))
  (:documentation "Default parental class for all objects in game."))

(defclass Creature ()
  ((display-character :initarg :display-character
                      :accessor display-character
                      :initform #\@)
   (name :initarg :name
         :accessor name)
   (description :initarg :description
                :accessor description)
   (location :initarg :location
             :accessor location)
   (hp :initarg :hp
       :accessor hp)
   (max-hp :initarg :max-hp
           :accessor max-hp)
   (wp :initarg :wp
       :accessor wp
       :documentation "Sanity. When this goes zero AI or player lose control on creature.")
   (max-wp :initarg :max-wp
           :accessor max-wp)
   (hunger :initarg :hunger
           :accessor hunger)
   (max-hunger :initarg :max-hunger
               :accessor max-hunger)
   (thirst :initarg :thirst
           :accessor thirst)
   (max-thirst :initarg :max-thirst
               :accessor max-thirst)
   (energy :initarg :energy
           :accessor energy)
   (max-energy :initarg :max-energy
               :accessor max-energy)
   (level :initarg :level
          :initform 0
          :accessor level)
   (attributes :initarg :attributes
               :accessor attributes
               :initform nil)
   (inventory :initarg :inventory
              :initform nil
              :accessor inventory)
   (hands :initarg :hands
          :initform (cons nil nil)
          :accessor hands
          :documentation "(left-hand . right-hand)")
   (clothes :initarg clothes
            :initform nil
            :accessor clothes))
  (:documentation "Living creature is object too of course."))

(defmethod initialize-instance :after ((creature Creature) &rest initargs)
  (declare (ignore initargs))
  (loop for slot in '(hp wp hunger thirst energy)
        for slot-max in '(max-hp max-wp max-hunger max-thirst max-energy)
        unless (slot-boundp creature slot)
          when (slot-boundp creature slot-max)
            do (setf (slot-value creature slot)
                     (slot-value creature slot-max))))


(defgeneric move-creature (creature map x y)
  (:documentation "Moves creature by `x' points on X axis and by `y' points on Y axis."))


(defmethod display-character ((object (eql nil)))
  #\Space)


(defun object-x (object)
  (first (location object)))

(defun object-y (object)
  (second (location object)))

(defun (setf object-x) (new-value object)
  (setf (first (location object)) new-value))

(defun (setf object-y) (new-value object)
  (setf (second (location object)) new-value))
