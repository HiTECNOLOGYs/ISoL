(in-package #:isol)

(defparameter *keys* nil
  "Contains bindings of keys to some actions.")

(defun bind-function-to-key (key function)
  (push (cons (char-code key) function)
        *keys*))

(defmacro define-key-binding (key function)
  "Creates action with some code and `name' and binds it to `key'."
  `(bind-function-to-key ,key
                         ,function))

(defmacro define-key-processor (key lambda-list &body body)
  `(bind-function-to-key ,key
                         #'(lambda ,lambda-list
                             ,@body)))

(defun get-key-processor (key)
  (cdr (assoc key *keys*
              :test #'=)))

(defun process-key (key &rest arguments)
  (when-let (key-processor (get-key-processor (if (integerp key)
                                                key
                                                (char-code key))))
    (apply key-processor arguments)))

(defun wait-for-key ()
  (cl-ncurses:getch))
