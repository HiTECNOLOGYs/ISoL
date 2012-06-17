;;;; map.lisp
;;;;
;;;; Author: Mark Fedurin <hitecnologys@gmail.com>
;;;; Description: Here is all map related code.

(defun normalize (list length)
  "Adds elements to list to make it's length equals `length'"
  (if (< (length list) length)
    (normalize (concatenate 'list list '(empty-cell)) length)
    list))

(defun normalize-list (list)
  "Adds elements to sublists in order ot make it's lengthes the same."
  (let ((max-size (loop for x in list
                        maximizing (length x) into maximum-length
                        finally (return maximum-length)))
        (new-list nil))
    (dolist (sublist list)
      (push (normalize sublist max-size) new-list))
    (reverse new-list)))

(defun find-list-size (list)
  "Calculates list size."
  (list (length list)
        (loop for x in list
              maximizing (length x) into maximum-length
              finally (return maximum-length))))

(defun list->array (list)
  "Converts list to 2d array."
  (let ((normalized-list (normalize-list list)))
    (make-array (find-list-size normalized-list)
                      :initial-contents normalized-list)))

;;; File format is (byte object-type) for now. So one byte is one object.
(defun load-map-from-file (path code->object-dict)
  "Loads map from binary file and use raw bytes as map objects."
  (with-open-file (file-stream path :element-type 'unsigned-byte)
    (list->array
      (reverse
        (loop for byte = (read-byte file-stream nil nil) while byte
              with buffer = nil
              with result-accamulator = nil
              if (equal byte 0)
                do (push (reverse buffer) result-accamulator)
                   (setf buffer nil)
              else
                do (push (funcall (cdr (assoc byte code->object-dict))) buffer)
              finally (return result-accamulator))))))

(defun check-passability (map x y)
  "Checks map cell passability."
  (object-passable (aref map x y)))