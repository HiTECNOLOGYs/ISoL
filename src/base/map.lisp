(in-package #:isol.map)

(defun find-list-size (list)
  "Calculates list size."
  (list (length list)
        (length (first list))))

(defun list->array (list)
  "Converts list to 2d array. A list should always be correct.
(all sublists should be equal)"
  (make-array (find-list-size list)
              :initial-contents list))

(defun load-map-from-file (path symbol->object-dictionary)
  "Loads map to memory as 2d array. Map is list of S-expressions. Each expressions
represents each row of the map."
  (with-open-file (file-stream path)
    (list->array
      (stream->list file-stream
                    (curry #'mapcar (curry #'symbol-object symbol->object-dictionary))))))

(defun check-passability (map x y)
  "Checks map cell passability."
  (object-passable (aref map y x)))

(defun render-map (map)
  "Transforms map in human-readable and printable form."
  (mapcar (curry #'mapcar #'object-display-character)
          (2d-array->list map)))
