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
  "Action that player can do.")
(defparameter *keyboard-hooks* nil
  "Contains bindings of keys to some actions.")

(defmacro defaction (name lambda-list &body body)
  "Defines new player action. `player' symbol binds to player that done action."
  `(progn (push ',name
                *player-actions*)
          (defun ,name ,(cons 'player lambda-list)
            ,@body)))

(defun bind-actinon-to-key (key action)
  "Binds action to some key."
  (push (cons key action)
        *keyboard-hooks*))

(defmacro defbinding ((name key) lambda-list &body body)
  "Creates action with some code and `name' and binds it to `key'."
  `(progn (defaction ,name ,lambda-list
            ,@body)
          (bind-actinon-to-key (char-code ,key)
                               ',name)))

(defun run-action (player name &rest arguments)
  "Runs action by it's name."
  (when (and (find name *player-actions*) (fboundp name))
    (apply name player arguments)))

(defun process-key (player key &rest arguments)
  "Finds action binded to some key and runs it."
  (when-let (action-name (cdr (assoc key *keyboard-hooks*)))
    (apply #'run-action player action-name arguments)))

(defaction move-player (map x y)
  "Moves player relative and checks map cell to passability."
  (destructuring-bind (player-x player-y) (player-position player)
    (when (check-passability map (+ player-x x) (+ player-y y))
      (incf player-x x)
      (incf player-y y))))
