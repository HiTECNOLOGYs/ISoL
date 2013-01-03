(in-package #:isol.utilities)

(defmacro when-let ((var form) &body body)
  "Binds predicate result to variable."
  `(let ((,var ,form))
     (when ,var
       ,@body)))

(defun curry (function &rest arguments)
  #'(lambda (&rest more-arguments)
      (apply function (append arguments more-arguments))))

(defun compose (function-1 function-2)
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
  (destructuring-bind (size-y size-x) (array-dimensions array)
    (loop for i below size-y
        collect (loop for j below size-x
                      collect (aref array i j)))))

(defun list->string (list)
  (coerce list 'string))

(defmacro doarray ((x-var y-var array) &body body)
  `(destructuring-bind (x-size y-size) (array-dimensions ,array)
     (dotimes (,y-var y-size)
       (dotimes (,x-var x-size)
         ,@body))))
