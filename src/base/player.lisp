(in-package #:isol)

(defstruct Player
  (position '(0 0))
  (character #\@))

(declaim (inline player-x))
(defun player-x (player)
  "Short binding for getting player X position."
  (first (player-position player)))

(declaim (inline player-y))
(defun player-y (player)
  "Short binding for getting player Y position."
  (second (player-position player)))

(defparameter *player-actions* nil
  "Contans all avaliable player actions.")
(defparameter *keyboard-hooks* nil
  "Contains bindings of keys to some actions.")

(defun add-action (name processor)
  "Binds new player action to some function."
  (push (list name processor)
        *player-actions*))

(defmacro defaction (name lambda-list &body body)
  "Defines new player action. First arguments is binded to player that done some action."
  `(add-action ',name
               #'(lambda ,lambda-list
                   ,@body)))

(defun bind-actinon-to-keys (keys action)
  "Binds action to some keys. When keys are pressed in the same time, action is called."
  (push (list keys action)
        *keyboard-hooks*))

(defmacro defbinding (name lambda-list keys &body body)
  "Creates action with some code and `name' and binds it to `keys'."
  `(progn (defaction ',name ,lambda-list ,@body)
          (bind-actinon-to-keys ,keys ',name)))

(defun run-action (player name &rest arguments)
  "Runs action by it's name."
  (when-let (action (second (assoc name *player-actions*)))
    (apply action player arguments)))

(defun process-keys (player keys &rest arguments)
  "Finds action binded to some keys and runs it."
  (when-let (action-name (second (assoc keys *keyboard-hooks* :test #'equalp)))
    (apply #'run-action player action-name arguments)))

(defaction move-player (player map x y)
  "Moves player relative and checks map cell to passability."
  (destructuring-bind (player-x player-y) (player-position player)
    (when (check-passability map (+ player-x x) (+ player-y y))
      (incf player-x x)
      (incf player-y y))))
