;;;; map.lisp
;;;;
;;;; Author: Mark Fedurin <hitecnologys@gmail.com>
;;;; Description: Here is all map related code.

;;; File format is (byte object-type) for now. So one byte is one object.
(defun load-map-from-file (path)
  "Loads map from binary file and use raw bytes as map objects."
  (with-open-file (file-stream path :element-type 'unsigned-byte)
    (reverse (loop for byte = (read-byte file-stream nil nil) while byte
                   with buffer = nil
                   with result-accamulator = nil
                   if (equal byte 0)
                     do (push (reverse buffer) result-accamulator)
                        (setf buffer nil)
                   else
                     do (push byte buffer)
                   finally (return result-accamulator)))))
