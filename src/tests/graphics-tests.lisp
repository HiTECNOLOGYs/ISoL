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
(in-suite graphics-tests)

(test test-attrubutes
  (is (= cl-ncurses:a_normal (get-attribute-name-from-keyword :normal)))
  (is (= cl-ncurses:a_blink (get-attribute-name-from-keyword :blink)))
  (is (= cl-ncurses:a_invis (get-attribute-name-from-keyword :invis))))
