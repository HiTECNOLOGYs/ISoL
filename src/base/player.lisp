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

(defclass Player (Creature)
  ((max-hp :initform 100)
   (max-wp :initform 100)
   (max-hunger :initform 1000)
   (max-thirst :initform 500)
   (max-energy :initform 10000))
  (:documentation "Player character."))

;;; **************************************************************************
;;;  Macros
;;; **************************************************************************

(defmacro with-relative-object ((map player object dx dy) &body body)
  "Finds object using coordinate system relative to player and binds this
object to `object'. Also binds object's true coordinates to X and Y."
  `(let* ((x (+ (object-x ,player) ,dx))
          (y (+ (object-y ,player) ,dy))
          (object (map-cell-top ,map x y)))
     ,@body))

;;; **************************************************************************
;;;  Info
;;; **************************************************************************

(defmacro define-encoder (name divisor &body values)
  "Defines function which returns values based on how close to maximum some
creature's parameter is. Resulting function accepts two arguments: value,
maximum-value."
  (with-gensyms (delta-gensym quarter-gensym)
    `(defun ,(symbol-append 'encode- name) (value max-value)
       (let ((,delta-gensym (- max-value value))
             (,quarter-gensym (/ max-value ,divisor)))
         (cond
           ((= ,delta-gensym max-value) ,(car (last values)))
           ,@(loop for (multiplier string) in (butlast values)
                   collecting `((<= ,delta-gensym (* ,multiplier ,quarter-gensym))
                                ,string)))))))

(define-encoder hp 4
  (0.25 "You're not injured at all")
  (0.5  "You have some little scratches")
  (0.75 "You have some scratches")
  (1    "You're injured")
  (2    "You're sightly injured")
  (2.5  "You're seriously injured")
  (3.5  "You're dying")
  (4    "You're almost dead")
        "You're dead, completely dead")

(define-encoder wp 4
  (0.25 "Your mind is clear")
  (0.5  "You feel weird")
  (0.75 "You feel dizzy")
  (1    "You feel nervours")
  (2    "Your head aches")
  (2.5  "You have severe headache")
  (3.5  "You're hallucinating")
  (4    "???")
        "You feel insane, you can't control your body")

(define-encoder hunger 4
  (0.25 "You're not hungry")
  (0.5  "Your stomach rumbles")
  (0.75 "You won't mind to eat something")
  (1    "You need to eat something")
  (2    "You really need to eat something")
  (2.5  "You feel uncontrollable desire to fill your stomach")
  (3.5  "Your stomach is killing you")
  (4    "You're almost starving")
        "You're starving")

(define-encoder thirst 4
  (0.25 "You're not thirsty")
  (0.5  "You feel dryness in your throat")
  (0.75 "You feel little thirst")
  (1    "You won't mind to drink something")
  (2    "You feel thirst")
  (2.5  "You really want to drink something")
  (3.5  "You fell weakness from dehydration")
  (4    "You're severely dehydrated")
        "You're dying of thirst")

(define-encoder energy 4
  (0.25 "You're full of energy!")
  (0.5  "You're not tired at all")
  (1    "You feel like you need a small nap")
  (2    "You feel a little bit tired")
  (2.5  "You're tired")
  (3.5  "You're very tired")
  (3.75 "You can't concentrate and walk")
  (4    "You can't stay awake")
        "You're completely exhausted")

(defun player-info (player)
  (list (encode-hp (hp player) (max-hp player))
        (encode-wp (wp player) (max-wp player))
        (encode-hunger (hunger player) (max-hunger player))
        (encode-thirst (thirst player) (max-thirst player))
        (encode-energy (energy player) (max-energy player))))

;;; **************************************************************************
;;;  Input modes
;;; **************************************************************************

(define-input-mode :game)
(define-input-mode :inventory)
(define-input-mode :object-usage)

;;; **************************************************************************
;;;  Movement
;;; **************************************************************************

(defmethod move-creature ((player Player) map x y)
  (when (map-cell-passable-p map (+ (object-x player) x) (+ (object-y player) y))
    (incf (object-x player) x)
    (incf (object-y player) y)
    (list (object-x player) (object-y player))))

(defmacro define-player-key-handler (name (key mode-name) &body body)
  `(define-key-handler ,name (,key ,mode-name)
     (with-slots (player map) game
       ,@body)))

(define-player-key-handler move-pc-up (#\j :game)
  (move-creature player map 0 1))

(define-player-key-handler move-pc-down (#\k :game)
  (move-creature player map 0 -1))

(define-player-key-handler move-pc-left (#\h :game)
  (move-creature player map -1 0))

(define-player-key-handler move-pc-right (#\l :game)
  (move-creature player map 1 0))

(define-player-key-handler move-pc-up-left (#\y :game)
  (move-creature player map -1 -1))

(define-player-key-handler move-pc-up-right (#\u :game)
  (move-creature player map 1 -1))

(define-player-key-handler move-pc-down-left (#\b :game)
  (move-creature player map -1 1))

(define-player-key-handler move-pc-down-right (#\n :game)
  (move-creature player map 1 1))

;;; **************************************************************************
;;;  Inventory
;;; **************************************************************************

(define-key-handler open-inventory (#\i :game)
  (multiple-value-bind (scene context)
      (inventory-scene :frame 'inventory-menu
                       :input-mode :inventory)
    (with-context context
      (setf (context-var :selected-item) 0)
      (push-scene scene game)
      (push-context context game)
      (display-scene scene))))

(define-key-handler close-inventory (#\q :inventory)
  (pop-context game)
  (display-scene (pop-scene game)))

(define-key-handler inventory-previous-tab (#\< :inventory)
  (cl-tui:tab-backwards 'inventory-menu))

(define-key-handler inventory-next-tab (#\> :inventory)
  (cl-tui:tab-forward 'inventory-menu))

(define-key-handler inventory-selection-down (#\j :inventory)
  (with-slots (inventory) (game-player game)
    (mod-incf (context-var :selected-item) (length inventory))))

(define-key-handler inventory-selection-up (#\k :inventory)
  (with-slots (inventory) (game-player game)
    (mod-decf (context-var :selected-item) (length inventory))))

(define-key-handler inventory-selection-left (#\h :inventory)
  ;; Move item to the lefter parts of the bag
  )

(define-key-handler inventory-selection-right (#\l :inventory)
  ;; Move item to the righter parts of the bag
  )

(define-key-handler inventory-confirm-selection (#\Newline :inventory)
  ;; Display info about item
  )

(define-key-handler inventory-selection-drop (#\d :inventory)
  (with-slots (inventory location) (game-player game)
    (destructuring-bind (x y) location
      (push-object (game-map game) x y (pop inventory)))
    (mod-decf (context-var :selected-item) (length inventory))))

;;; **************************************************************************
;;;  Other objects manipulations
;;; **************************************************************************

;; ----------------
;; Picking up

(defmethod pick-up-object ((player Player) map)
  (with-relative-object (map player object 0 0)
    (when (and object (takable-p object))
      (push (pop-object map x y) (inventory player)))))

(define-player-key-handler pc-pick-object (#\; :game)
  (pick-up-object player map))

;; ----------------
;; Using world objects

(define-player-key-handler pc-use-object (#\a :game)
  (push-context (make-context :input-mode :object-usage)
                *game*))

(define-player-key-handler pc-use-object-up (#\j :object-usage)
  (with-relative-object (map player object 0 1)
    (use-object player map object))
  (pop-context *game*))

(define-player-key-handler pc-use-object-down (#\k :object-usage)
  (with-relative-object (map player object 0 -1)
    (use-object player map object))
  (pop-context *game*))

(define-player-key-handler pc-use-object-left (#\h :object-usage)
  (with-relative-object (map player object -1 0)
    (use-object player map object))
  (pop-context *game*))

(define-player-key-handler pc-use-object-right (#\l :object-usage)
  (with-relative-object (map player object 1 0)
    (use-object player map object))
  (pop-context *game*))

(define-player-key-handler pc-use-object-up-left (#\y :object-usage)
  (with-relative-object (map player object -1 -1)
    (use-object player map object))
  (pop-context *game*))

(define-player-key-handler pc-use-object-up-right (#\u :object-usage)
  (with-relative-object (map player object 1 -1)
    (use-object player map object))
  (pop-context *game*))

(define-player-key-handler pc-use-object-down-left (#\b :object-usage)
  (with-relative-object (map player object -1 1)
    (use-object player map object))
  (pop-context *game*))

(define-player-key-handler pc-use-object-down-right (#\n :object-usage)
  (with-relative-object (map player object 1 1)
    (use-object player map object))
  (pop-context *game*))

;; ----------------
;; Methods

(defmethod use-object ((creature Player) map (object (eql nil)))
  (display-message *game* "Err. There's NOTHING in here. Literally."))

(defmethod use-object ((creature Player) map object)
  (display-message *game*
                   "I have no idea how am I supposed to make use of ~A."
                   (name object)))

(defmethod use-object ((creature Player) map (object Door))
  (with-slots (open passable? display-character
               display-character-open display-character-closed)
      object
    (setf open (not open)
          passable? open
          display-character (if open
                              display-character-open
                              display-character-closed))
    (display-message *game*
                     (if open
                       "The door is now opened."
                       "The door is now closed."))
    open))
