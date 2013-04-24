(in-package #:isol)

(defclass Object ()
  ((name :initarg :name
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
   (display-character :initarg :display-character
                      :accessor display-character
                      :initform #\0)
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
   (wp :initarg :wp
       :accessor wp
       :documentation "Will points. Decreases if creature is under stress or something. Determines lots of stuff.")
   (level :initarg :level
          :initform 0
          :accessor level)
   (attributes :initarg :attributes
               :accessor attributes)
   (inventory :initarg :inventory
              :accessor inventory)
   (hands :initarg :hands
          :accessor hands)
   (clothes :initarg clothes
            :accessor clothes))
  (:documentation "Living creature is object too of course."))


(defgeneric move-creature (creature map x y)
  (:documentation "Moves creature by `x' points on X axis and by `y' points on Y axis."))
