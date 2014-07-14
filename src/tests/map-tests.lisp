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
(in-suite map-tests)

(defmacro check-cell (array x y value)
  `(is (eq (aref ,array ,y ,x) ,value)))

(test test-symbol-conversion
  (let ((instance (get-object-instance-from-symbol :wall)))
    (is (equal (name instance)
               "Wall"))
    (is (equal (description instance)
               "Just rusty old stone wall."))
    (is (= (hp instance)
               10000))
    (is (eql (material instance)
             'stone))))

(test test-objects-manipulations
  (with-test-map (map)
    (is (push-object map 1 1 (get-object-instance-from-symbol :gun)))
    (is (and (eql (class-of (get-map-cell-top map 1 1))
                  (class-of (make-instance 'Weapon)))
             (equal (name (get-map-cell-top map 1 1))
                    "Revolver")))
    (is (push-object map 1 2 (get-object-instance-from-symbol :rock)))
    (is (and (eql (class-of (get-map-cell-top map 1 2))
                  (class-of (make-instance 'Map-Object)))
             (equal (name (get-map-cell-top map 1 2))
                    "Rock")))
    (is (equal (name (pop-object map 1 1))
               "Revolver"))
    (is (equal (name (pop-object map 1 2))
               "Rock"))))

(test (test-map-loading :depends-on test-symbol-conversion)
  (with-test-map (map)
    (is       (map-cell-passable-p map 1 1))
    (is-false (map-cell-passable-p map 0 0))))

(test (test-map-rendering :depends-on test-map-loading)
  (with-test-rendered-map (rendered-map)
    (is (eq (first (first rendered-map)) #\#))
    (is (eq (second (second rendered-map)) #\.))))

