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

(defparameter *game* nil
  "Stores current game instance.")

(bind-key #\q #'(lambda (game)
                  (declare (ignore game))
                  (throw 'exit-game (values))))

(defun run-game (game)
  "Runs game."
  (push-scene (make-scene (curry #'game-scene game)) game)
  (setf (game-map game) (load-map-from-file (make-pathname :directory '(:relative "res")
                                                           :name "test-map"
                                                           :type "isol")))
  (push-object (game-map game) 2 2 (get-object-instance-from-symbol :gun))
  (push-object (game-map game) 3 13 (get-object-instance-from-symbol :rock))
  (push-object (game-map game) 1 9 (get-object-instance-from-symbol :knife))
  (push-object (game-map game) 2 2 (get-object-instance-from-symbol :long-name))
  (catch 'exit-game
    (handler-case
      (cl-tui:with-screen (:noecho :nocursor :cbreak)
        (display 'game-scene)
        (redraw-screen)
        (unless (screen-size-sufficient-p)
          (redraw-screen)
          (put-text :root 0 0 "To play ISoL you need at least 40 rows and 80 columns in your terminal, sorry. To quit press ^C.")
          (take-a-nap))
        ;(put-text 'minibuffer 0 0 "Welcome to ISoL ~A" (asdf:component-version (asdf:find-system :isol)))
        (loop (game-tick game)
              (sleep 1/100)))
      (sb-sys:interactive-interrupt ()
        (throw 'exit-game (values))))))

(defun main ()
  "Start point."
  (setf *game* (make-game))
  (run-game *game*))
