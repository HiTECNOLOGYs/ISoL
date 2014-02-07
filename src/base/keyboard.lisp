(in-package :isol)

(defparameter *keys* nil
  "Contains bindings of keys to some actions.")

(defun bind-function-to-key (key function)
  "Assigns function to key."
  (pushnew (cons (char-code key) function)
           *keys*
           :test #'=
           :key #'car))

(defmacro define-key-binding (key function)
  "Creates action with some code and `name' and binds it to `key'."
  `(bind-function-to-key ,key
                         ,function))

(defmacro define-key-processor (key lambda-list &body body)
  "Defines function and sets it as key processor."
  `(bind-function-to-key ,key
                         #'(lambda ,(append '(player map) lambda-list)
                             ,@body)))

(defun clear-key-bindings ()
  "Removes all key bindings."
  (setf *keys* nil))

(defun get-key-processor (key)
  "Returns function binded to some key or NIL."
  (cdr (assoc key *keys*
              :test #'=)))

(defun process-key (key player map &rest arguments)
  "Runs function which is binded to some key."
  (when-let (key-processor (get-key-processor (if (integerp key)
                                                key
                                                (char-code key))))
    (apply key-processor player map arguments)))

(defun wait-for-key ()
  "Waits for user presses key."
  (getch))
