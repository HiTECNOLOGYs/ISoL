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
(in-suite player-tests)

(defun gen-integer (&key (max (1+ most-positive-fixnum)) (min (1- most-negative-fixnum)))
  (+ min (random (1+ (- max min)))))

(test test-player-location
  (let* ((x (gen-integer))
         (y (gen-integer))
         (player (make-instance 'player
                                :location (list x y))))
    (is (= (first (location player)) x))
    (is (= (second (location player)) y))))

(test test-keybindigns
  (let ((some-random-integer (gen-integer))
        (some-random-char (code-char (gen-integer :min 20 :max 128))))
    (define-key-processor some-random-char (some-number)
      (declare (ignore player map))
      (is (= some-number some-random-integer)))
    (process-key some-random-char nil nil some-random-integer)
    (clear-key-bindings)))
