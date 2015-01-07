;;; Copyright (C) Mark Fedurin, 2011-2015.
;;;
;;; This file is part of ISoL.
;;;
;;; ISoL is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; ISoL is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with ISoL.  If not, see <http://www.gnu.org/licenses/>.

(in-package :isol)

;;; **************************************************************************
;;;  Filesystem
;;; **************************************************************************

(defparameter *system-dir* (asdf:component-pathname (asdf:find-system :isol)))

(defun get-resources-dir-pathname ()
  (merge-pathnames #p"../res/" *system-dir*))

(defun resource (pathname)
  (merge-pathnames pathname (get-resources-dir-pathname)))

;;; **************************************************************************
;;;  Images
;;; **************************************************************************

;;; Images are loaded directly in foreign (C) arrays as I can't possibly image
;;; the cases where I would need them to be Lisp arrays.
(defclass Image ()
  ((data :initarg :data)
   (width :initarg :width)
   (height :initarg :height)
   (channels :initarg :channels)
   (format :initarg :format)
   (pathname :initarg :pathname)))

;;; Currently only PNG files are supported because I'm lazy
;;; TODO Implement support for other file formats
(defun load-image (pathname)
  (let ((img (opticl:read-png-file pathname)))
    (opticl:with-image-bounds (height width channels) img
      (make-instance 'Image
                     :data img
                     :width width
                     :height height
                     :channels channels
                     :format (typecase img
                               (opticl:8-bit-rgb-image :rgb)
                               (opticl:8-bit-rgba-image :rgba))
                     :pathname pathname))))
