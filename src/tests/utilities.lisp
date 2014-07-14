;;; Copyright (C) Mark Fedurin, 2011-2014.
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

(define-constant +test-map-path+ (merge-pathnames
                                   (make-pathname :directory '(:relative "res")
                                                  :name "test-map"
                                                  :type "isol")
                                   #.(or *compile-file-truename* *load-truename*)))

(defmacro with-test-map ((map-variable) &body body)
  `(let ((,map-variable (load-map-from-file +test-map-path+)))
     ,@body))

(defmacro with-test-rendered-map ((variable) &body body)
  (let ((map-variable (gensym)))
    `(with-test-map (,map-variable)
       (let ((,variable (render-map ,map-variable)))
         ,@body))))
