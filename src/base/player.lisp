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
;;;  Movement
;;; **************************************************************************

(defmethod move-creature ((player Player) map x y)
  (when (map-cell-passable-p map (+ (object-x player) x) (+ (object-y player) y))
    (incf (object-x player) x)
    (incf (object-y player) y)
    (list (object-x player) (object-y player))))

(defmacro define-player-key (char &body body)
  `(bind-key ,char
             #'(lambda (game)
                 (with-slots (player map) game
                   ,@body))))

(define-player-key #\j
  (move-creature player map 0 1))

(define-player-key #\k
  (move-creature player map 0 -1))

(define-player-key #\h
  (move-creature player map -1 0))

(define-player-key #\l
  (move-creature player map 1 0))

(define-player-key #\y
  (move-creature player map -1 -1))

(define-player-key #\u
  (move-creature player map 1 -1))

(define-player-key #\b
  (move-creature player map -1 1))

(define-player-key #\n
  (move-creature player map 1 1))

;;; **************************************************************************
;;;  Inventory
;;; **************************************************************************

(defun close-inventory (game)
  (display-scene (pop-scene game)))

(defun open-inventory (game)
  (let* ((scene (inventory-scene :frame 'inventory-menu
                                 :keys '((#\q close-inventory)
                                         (#\< inventory-previous-tab)
                                         (#\> inventory-next-tab)
                                         (#\j inventory-selection-down)
                                         (#\k inventory-selection-up)
                                         (#\h inventory-selection-left)
                                         (#\l inventory-selection-right)
                                         (#\Newline inventory-confirm-selection))))
         (context (scene-context scene)))
    (with-context context
      (setf (context-var :selected-item) 0)
      (push-scene scene game)
      (display-scene scene))))

(defun inventory-previous-tab (game)
  (declare (ignore game))
  (cl-tui:tab-backwards 'inventory-menu))

(defun inventory-next-tab (game)
  (declare (ignore game))
  (cl-tui:tab-forward 'inventory-menu))

(defun inventory-selection-down (game)
  (with-slots (inventory) (game-player game)
    (setf (context-var :selected-item)
          (mod (+ (context-var :selected-item) 1)
               (length inventory)))))

(defun inventory-selection-up (game)
  (with-slots (inventory) (game-player game)
    (setf (context-var :selected-item)
          (mod (+ (context-var :selected-item) -1)
               (length inventory)))))

(defun inventory-selection-right (game)
  (declare (ignore game))
  ;; Display some info about item here
  )

(defun inventory-selection-left (game)
  (declare (ignore game))
  ;; Do something else
  )

(defun inventory-confirm-selection (game)
  (declare (ignore game))
  ;; Display extensive info about item
  )

;;; **************************************************************************
;;;  Other objects manipulations
;;; **************************************************************************

(defmethod pick-up-object ((player Player) map)
  (destructuring-bind (x y) (location player)
    (awhen (map-cell-top map x y)
      (when (takable-p it)
        (push (pop-object map x y)
              (inventory player))))))

(define-player-key #\;
  (pick-up-object player map))

(bind-key #\i 'open-inventory)
