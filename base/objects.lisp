;;;; objects.lisp
;;;;
;;;; Author: Mark Fedurin <hitecnologys@gmail.com>.
;;;; Description: Here is all objects related code.

(defstruct Object
  (display-symbol #\O)
  (passable nil))

(defun object-x (object)
  "Short binding for getting object X position."
  (first (object-position object)))

(defun object-y (object)
  "Short binding for getting object Y position."
  (second (object-position object)))

(defun != (num1 num2)
  "Does the same as (not (equal `num1' `num2')). USE ONLY WITH NUMBERS."
  (not (equal num1 num2)))

(declaim (inline object-x object-y !=))

(defun parse-object-type (line)
  "Parses s-expression and return lambda for object creation."
  (when (!= (length line) 4)
    (return-from parse-object-type #'(lambda () nil)))
  (cons (second line)
        #'(lambda ()
            (make-object :display-symbol (third line)
                         :passable (fourth line)))))

;;; Object type format is:
;;; name map-code display-symbol passability
(defun load-objects-list-from-file (path)
  "Loads object types list from file."
  (with-open-file (file-stream path)
    (loop for line = (read file-stream nil nil)
          while line
          collecting (parse-object-type line))))