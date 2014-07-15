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

(defvar *game*)
(defvar *game-version*)

(defun exit-game (game)
  (declare (ignore game))
  (throw 'exit-game (values)))

(defun no-way ()
  (put-text :root 0 0 "To play ISoL you need at least 40 rows and 80 columns in your terminal, sorry. To quit press ^C.")
  (take-a-nap))

(defun game-loop ()
 (loop (game-tick *game*)
       (sleep 1/100)))

(defun run-game ()
  "Runs game."
  (catch 'exit-game
    (handler-case
      (cl-tui:with-screen (:noecho :nocursor :cbreak)
        (redraw-screen)
        (unless (screen-size-sufficient-p)
          (no-way))
        (log-game-message *game* "Welcome to ISoL ~A" *game-version*)
        (game-loop))
      (sb-sys:interactive-interrupt ()
        (exit-game *game*)))))

(defun main ()
  "Start point."
  (let* ((game (new-game :map (gen-new-map :testing)))
         (game-version (asdf:component-version (asdf:find-system :isol)))
         (scene (make-scene #'game-scene
                            :keybindings '((#\q exit-game))
                            :variables `((*game* ,game)
                                         (*game-version* ,game-version)))))
    (push-scene scene game)
    (run-game)))
