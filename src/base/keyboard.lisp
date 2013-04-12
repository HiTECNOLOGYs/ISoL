(in-package #:isol)

(defparameter *keys* nil
  "Contains bindings of keys to some actions.")

(defparameter *prefixes* nil
  "List of keys prefixes and function which are bindined to them.
When prefix is pressed, the next key processor result will be passed stright to prefix processor.")

(defun bind-function-to-key (key function)
  (push (cons (char-code key) function)
        *keys*))

(defun bind-function-to-key-prefix (prefix function)
  (push (cons (char-code prefix function))
        *prefixes*))

(defmacro define-key-binding (key function)
  "Creates action with some code and `name' and binds it to `key'."
  `(bind-function-to-key ,key
                         ,function))

(defmacro define-key-processor ((function key) lambda-list &body body)
  `(bind-function-to-key ,key
                         (defun ,function ,lambda-list
                           ,@body)))

(defmacro define-key-prefix (prefix function)
  `(bind-function-to-key-prefix ,key
                                ,function))

(defmacro define-key-prefix-processor ((function prefix) lambda-list &body body)
  `(bind-function-to-key-prefix ,key
                                (defun ,function ,lambda-list
                                  ,@body)))

(defun wait-for-key ()
  (cl-ncurses:getch))

(defun process-key (key &rest arguments)
  "Finds action binded to some key and runs it."
  (if-let ((prefix-processor (find key *prefixes*)))
    (apply (cdr prefix-processor)
           (process-key (wait-for-key)))
    (when-let (key-processor (cdr (assoc key *keys*)))
      (apply key-processor
             arguments))))
