(in-package #:isol)

(defclass Map-Object (Object)
  ((display-character :initform #\&)
   (takable? :initform nil)
   (movable? :initarg :movable?
             :accessor movable-p))
  (:documentation "Object whcih may not be fixed on map, may be even 'transparent' for player motions. But it is always untakable."))

(defclass Map-Element (Object)
  ((display-character :initform #\#)
   (takable? :initform nil)
   (passable? :initform nil))
  (:documentation "Object on map which is not movable as it is a part of world."))

(defun find-list-size (list)
  "Calculates list size."
  (list (length list)
        (length (first list))))

(defun list->array (list)
  "Converts list to 2d array. A list should always be correct. (all sublists should be equal)"
  (make-array (find-list-size list)
              :initial-contents list))

(defun load-map-from-file (path)
  "Loads map to memory as 2d array. Map is list of S-expressions. Each expressions represents each row of the map."
  (with-open-file (file-stream path)
    (list->array
      (stream->list file-stream
                    (curry #'mapcar #'get-object-instance-from-symbol)))))

(defun render-map (map)
  "Transforms map in human-readable and printable form."
  (mapcar (curry #'mapcar #'display-character)
          (2d-array->list map)))

(defun map-cell-passable-p (map x y)
  "Checks if some map place is passable for player."
  (passable-p (aref map y x)))


(defparameter *objects-map-reader-symbols* nil)

(defmacro define-object-map-symbol (symbol class &body default-initargs)
  `(push (cons ,symbol
               #'(lambda (&rest initargs)
                   (if initargs
                     (apply #'make-instance ',class
                            ,@default-initargs initargs)
                     (make-instance ',class ,@default-initargs))))
         *objects-map-reader-symbols*))

(defun get-object-instance-from-symbol (symbol &rest initargs)
  (when-let (function (cdr (assoc symbol *objects-map-reader-symbols*)))
    (apply function initargs)))

(define-object-map-symbol :Wall Map-Element
  :name "Wall"
  :description "Just rusty old stone wall."
  :display-character #\#
  :hp 10000
  :material 'stone)

(define-object-map-symbol :Ground Map-Element
  :name "Ground"
  :passable? t
  :description "Nothing in here."
  :display-character #\.
  :hp :100000
  :material 'stone)
