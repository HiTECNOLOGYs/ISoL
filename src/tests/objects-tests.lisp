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
(in-suite objects-tests)

(test creature-initialization
  (let ((creature (make-instance 'creature
                                 :max-hp 100
                                 :max-wp 101
                                 :max-hunger 102
                                 :max-thirst 103
                                 :max-energy 104)))
    (is (= (hp creature) 100))
    (is (= (wp creature) 101))
    (is (= (hunger creature) 102))
    (is (= (thirst creature) 103))
    (is (= (energy creature) 104))))
