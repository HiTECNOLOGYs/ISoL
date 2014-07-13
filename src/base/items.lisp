(in-package :isol)

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


(defgeneric use-object (creature map item)
  (:documentation "Called each time item is used by creature."))
(defgeneric pick-up-object (creature map)
  (:documentation "Called each time creature picks up object."))
(defgeneric combine-objects (creature item-1 item-2)
  (:documentation "Called whenever creature tries to combine two items."))

(defmethod print-object ((item Item) (stream (eql :minibuffer)))
  (display-message-in-minibuffer (format nil "~A is lying here." (name item))))
