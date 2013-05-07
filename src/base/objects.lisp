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
       :documentation "Sanity. When this goes zero player looses control on PC.")
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
          :initform nil
          :accessor hands)
   (clothes :initarg clothes
            :initform nil
            :accessor clothes))
  (:documentation "Living creature is object too of course."))

(defmethod initialize-instance :after ((creature Creature) &rest initargs)
  (declare (ignore initargs))
  (dolist (slot '(hp wp hunger thirst energy))
    (unless (slot-boundp creature slot)
      (setf (slot-value creature slot)
            (slot-value creature (intern (concatenate 'string
                                                      "MAX-"
                                                      (symbol-name slot))))))))


(defgeneric move-creature (creature map x y)
  (:documentation "Moves creature by `x' points on X axis and by `y' points on Y axis."))


(defmethod display-character ((object (eql nil)))
  #\Space)
