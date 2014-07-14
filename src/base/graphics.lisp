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

;;; **************************************************************************
;;;  Callbacks
;;; **************************************************************************

(defun draw-map (frame map)
  (iter
    (for line in (render-map map))
    (for y from 1)
    (after-each
      (iter
        (for char in line)
        (for x from 1)
        (after-each
          (cl-tui:put-char frame y x char))))))

(defun draw-player (frame player)
  (with-slots (display-character location) player
    (destructuring-bind (x y) location
      (put-char frame (1+ x) (1+ y) display-character))))

(defun draw-player-info (frame player)
  (iter
    (for line in (player-info player))
    (for y from 1)
    (after-each
      (put-text frame 1 y line))))

(defun game-map-callback (&key frame h w)
  (cl-tui:draw-box frame)
  (draw-map 'game-map (game-map *game*))
  (draw-player 'game-map (game-player *game*)))

(defun player-info-callback (&key frame h w)
  (cl-tui:draw-box frame)
  (draw-player-info 'player-info (game-player *game*)))

(defun minibuffer-callback (&key frame h w)
  (cl-tui:draw-box frame)
  (with-slots (map player) *game*
    (destructuring-bind (x y) (location player)
      (let ((map-cell (get-map-cell-top map x y)))
        (when (typep map-cell 'Item)
          (put-text frame 1 1 "~A is lying here." (name map-cell)))))))

(defun game-log-callback (&key frame h w)
  (cl-tui:draw-box frame)
  ;; Display last log entries here.
  ;; This log is the only thing besides minibuffer that can thell player what
  ;; the hell is going on. I should probably do some kind of text colorizing
  ;; and stuff here to make it more informative.
  )
