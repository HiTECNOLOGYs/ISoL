(in-package :isol)

(defun stream->list (stream &optional (s-expressions-processor nil processor-passed?))
  "Reads stream data to list of S-expressions."
  (loop for result = (read stream nil nil) while result
        if processor-passed?
          collecting (funcall s-expressions-processor result)
        else
          collecting result))

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

(defun symbol-append (symbol-1 symbol-2 &optional (package *package*))
  (intern (concatenate 'string
                       (symbol-name symbol-1)
                       (symbol-name symbol-2))
          package))
