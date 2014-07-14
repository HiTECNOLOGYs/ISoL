(in-package :isol)

(defvar *keys* (make-hash-table)
  "Contains bindings of keys to some actions.")

(defun key-binding (key)
  "Returns function binded to given key."
  (gethash key *keys*))

(defun (setf key-binding) (function key)
  (setf (gethash (char-code key) *keys*) function))

(defun bind-key (key function)
  "Binds given character to some function."
  (setf (key-binding key) function))

(defun clear-key-bindings ()
  "Removes all key bindings."
  (clrhash *keys*))

(defun process-key (key &rest arguments)
  "Runs function binded to some `key' with given arguments."
  (awhen (key-binding (etypecase key
                        (integer key)
                        (character (char-code key))
                        (symbol key)))
    (apply it arguments)))

(defun wait-for-key ()
  "Waits until user presses key and returns it."
  (cl-tui:read-key))

(defmacro with-temporary-key-bindings (new-bindings &body body)
  "Binds *keys* to `new-bindings' and evalutes `body' in this environment."
  `(let ((*keys* ,(alist-hash-table (iter
                                      (for (character function) in new-bindings)
                                      (collecting (cons (char-code character)
                                                        function))))))
     ,@body))
