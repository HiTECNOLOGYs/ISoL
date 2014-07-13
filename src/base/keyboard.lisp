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
  "Defines function and sets it as `key' processor."
  `(bind-function-to-key ,key
                         #'(lambda ,(append '(player map) lambda-list)
                             ,@body)))

(defun clear-key-bindings ()
  "Removes all key bindings."
  (setf *keys* nil))

(defun get-key-processor (key)
  "Returns function binded to some `key' or NIL if no binding was found."
  (cdr (assoc key *keys*
              :test #'=)))

(defun process-key (key &rest arguments)
  "Runs function binded to some `key' with given arguments."
  (when-let (key-processor (get-key-processor (if (integerp key)
                                                key
                                                (char-code key))))
    (apply key-processor arguments)))

(defun wait-for-key ()
  "Waits until user presses key and returns it."
  (getch))

(defmacro with-temporary-key-bindings (new-bindings &body body)
  "Binds *keys* to `new-bindings' and evalutes `body' in this environment."
  `(let ((*keys* (list ,@(loop for (character function) in new-bindings
                               collecting `(cons ,(char-code character)
                                                 ,function)))))
     ,@body))
