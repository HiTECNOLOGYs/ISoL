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
;;;  Base
;;; **************************************************************************

;; ----------------
;; Classes

(defclass Item (Object)
  ((display-character :initform #\?)
   (takable? :initform t)
   (passable? :initform t)
   (size :initarg :size
         :accessor size)
   (weight :initarg :weight
           :accessor weight))
  (:documentation "Takable stuff which player can use for great justice."))

(defclass Weapon (Item)
  ((display-character :initform #\()
   (damage-value :initarg :damage-value
                 :accessor damage-value)
   (modifiers :initarg :modifiers
              :accessor modifiers)
   (kind :initarg :kind
         :accessor kind)))

(defclass Tool (Item)
  ((display-character :initform #\))
   (hook :initarg :hook
         :accessor hook)))

(defclass Cloth (Item)
  ((display-character :initform #\])
   (defence-value :initarg :defence-value
                  :accessor defence-value)
   (body-slot :initarg :body-slot
              :accessor body-slot)))

(defclass Container (Item)
  ((display-character :initform #\%)
   (volume :initarg :volume
           :accessor container)
   (size :initarg :size
         :accessor size)
   (items :initarg :items
          :accessor items))
  (:documentation "This is like additional inventory. Main indevtory is limited in size so this stuff cound be very useful.")) 

;; ----------------
;; Generic functions

(defgeneric use-object (creature map item)
  (:documentation "Called each time item is used by creature."))
(defgeneric pick-up-object (creature map)
  (:documentation "Called each time creature picks up object."))
(defgeneric combine-objects (creature item-1 item-2)
  (:documentation "Called whenever creature tries to combine two items."))
