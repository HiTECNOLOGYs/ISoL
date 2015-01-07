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

(in-package :isol.tests)
(in-suite :isol.tests.engine)

(test contexts
  (let ((context (isol::make-context :test "foo")))
    (isol::with-context (context)
      (is (equal "foo" (isol::context-var :test))))
    (isol::with-context ((isol::copy-context context :test "bar" :foo "foo"))
      (is (equal "bar" (isol::context-var :test)))
      (is (equal "foo" (isol::context-var :foo))))))
