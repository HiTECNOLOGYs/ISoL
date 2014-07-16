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

;;; **************************************************************************
;;;  Some stuff
;;; **************************************************************************

(defun get-screen-size ()
  "Returns size of terminal screen."
  (cl-tui:frame-size))

(defun get-screen-center ()
  (destructuring-bind (x y) (get-screen-size)
    (list (truncate x 2)
          (truncate y 2))))

(defun screen-size-sufficient-p ()
  (destructuring-bind (y x) (get-screen-size)
    (and (<= 80 x) (<= 40 y))))

(defun clear-screen ()
  (cl-tui:clear cl-tui::*display*))

(defun redraw-screen ()
  (cl-tui:refresh))

(defun display (&optional (frame :root))
  (cl-tui:display frame))

(defun put-text (frame x y format-string &rest format-args)
  (apply #'cl-tui:put-text frame y x format-string format-args))

(defun put-char (frame x y char)
  (cl-tui:put-char frame y x char))

(defun display-message-in-minibuffer (format-string &rest format-args)
  (let ((minibuffer-width (second (cl-tui:frame-size (cl-tui:frame 'minibuffer))))
        (string (apply #'format nil format-string format-args)))
    (put-text 'minibuffer 0 0 (ensure-string-within-length minibuffer-width string))))

;;; **************************************************************************
;;;  Callbacks
;;; **************************************************************************

;; ----------------
;; Helper functions

(defun draw-map (frame map)
  (iter
    (for line in (render-map map))
    (for y from 0)
    (after-each
      (iter
        (for char in line)
        (for x from 0)
        (after-each
          (cl-tui:put-char frame y x char))))))

(defun draw-player (frame player)
  (with-slots (display-character location) player
    (destructuring-bind (x y) location
      (put-char frame x y display-character))))

(defun draw-player-info (frame player)
  (iter
    (for line in (player-info player))
    (for y from 0)
    (after-each
      (put-text frame 0 y line))))

;; ----------------
;; Callbacks

(defun game-map-callback (&key frame)
  (draw-map frame (game-map *game*))
  (draw-player frame (game-player *game*)))

(defun player-info-callback (&key frame)
  (draw-player-info frame (game-player *game*)))

(defun game-log-callback (&key frame h)
  (loop
    for message in (first-n *log-backtrack-n* (game-log *game*))
    with y = 0
    do
    (when message
      (incf y)
      (put-text frame 0 y (ensure-string-within-length h message)))))

(defun items-callback (&key frame)
  (iter
    (for item in (inventory (game-player *game*)))
    (for i from 0)
    (after-each
      ;; x = 1 is just for small nice padding, it's temporary
      (put-text frame 1 i "[~D] ~A" i (name item)))))
