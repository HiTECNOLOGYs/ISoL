(in-package #:isol)

(defstruct Object
  (display-character #\O)
  (passable nil))

(defun symbol-object (dictionary symbol)
  "Retuns object for some specific `symbol'.
Object is created by calling function associated with `symbol' from `dictionary'."
  (funcall (cdr (assoc symbol dictionary))))

(defun parse-object-type (line)
  "Parses S-expression and return closure for object creation."
  (destructuring-bind (id description character passable) line
    (declare (ignore description))
    (cons id
          #'(lambda ()
              (make-object :display-character character
                           :passable passable)))))

(defun load-objects-list-from-file (path)
  "Loads object types list from file."
  (with-open-file (file-stream path)
    (stream->list file-stream #'parse-object-type)))
