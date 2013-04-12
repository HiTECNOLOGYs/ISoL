(in-package #:isol)

(defmacro if-let ((var form) true &optional not-true)
  `(let ((,var ,form))
     (if ,var
       not-true)))

(defmacro when-let ((var form) &body body)
  "Binds predicate result to variable."
  `(if-let (,var ,form)
     (progn ,@body)))

(defun curry (function &rest arguments)
  "Partially applies arguments to function."
  #'(lambda (&rest more-arguments)
      (apply function (append arguments more-arguments))))

(defun compose (function-1 function-2)
  "Makes lambda of two function where `function-2' result is passed to `function-1'."
  #'(lambda (&rest arguments)
      (funcall function-1 (apply function-2 arguments))))

(defun stream->list (stream &optional (s-expressions-processor nil processor-passed-p))
  "Reads stream data to list of S-expressions."
  (loop :for result = (read stream nil nil) :while result
        :if processor-passed-p
          :collecting (funcall s-expressions-processor result)
        :else
          :collecting result))

(defun 2d-array->list (array)
  "Converts 2 dimensional array to list of lists whcih are rows of array."
  (destructuring-bind (size-y size-x) (array-dimensions array)
    (loop for i below size-y
        collect (loop for j below size-x
                      collect (aref array i j)))))

(defun list->string (list)
  "Converts list of characters to string."
  (coerce list 'string))

(defmacro doarray ((x-var y-var array) &body body)
  "Iterates through `array' from top left angle to down right angle.
Binds `x-var' to current x position and `y-var' to current y position."
  `(destructuring-bind (x-size y-size) (array-dimensions ,array)
     (dotimes (,y-var y-size)
       (dotimes (,x-var x-size)
         ,@body))))
