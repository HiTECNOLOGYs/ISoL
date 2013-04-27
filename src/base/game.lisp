(in-package #:isol)

(defstruct Game
  map
  (player (make-instance 'player :location (list 1 1)))
  (creatures (make-hash-table)))

(defun game-step (game)
  (print-map (game-map game))
  (write (game-player game)
         :stream :game-window)
  (write (game-player game)
         :stream :info-window)
  (redraw-screen)
  (process-key (wait-for-key)
               (game-player game)
               (game-map game)))


(defparameter *game* (make-game))

(define-condition exit-game () ())

(define-key-processor #\q ()
  (declare (ignore player map))
  (error 'exit-game))

(defun run-game ()
  "Runs game."
  (setf (game-map *game*)
        (load-map-from-file (make-pathname :directory '(:relative "res")
                                           :name "test-map"
                                           :type "isol")))
  (push-object (game-map *game*) 3 2 (get-object-instance-from-symbol :gun))
  (with-screen (:noecho :nocursor :cbreak)
    (clear-screen)
    (create-new-window :game-window 0 0 30 30)
    (create-new-window :info-window 30 0 20 30)
    (redraw-screen)
    (draw-window-box :game-window)
    (draw-window-box :info-window)
    (catch 'end-game
      (handler-case (loop (game-step *game*)
                          (sleep 1/100))
        (exit-game ()
          (throw 'end-game (values)))
        (sb-sys:interactive-interrupt ()
          (throw 'end-game (values)))))))
         
