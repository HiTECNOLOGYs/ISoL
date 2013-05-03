(in-package :isol)

(define-constant +info-window-size+ 20)
(define-constant +minibuffer-size+ 2)
(defparameter *screen-size* nil)

(defstruct Game
  map
  (player (make-instance 'player :location (list 1 1)))
  (creatures (make-hash-table)))

(defun game-tick (game)
  (reset-all-windows-cursor-positions)
  (print-map (game-map game))
  (write (game-player game)
         :stream :game-window)
  (write (game-player game)
         :stream :info-window)
  (let ((map-cell (get-map-cell-top (game-map game)
                                    (player-x (game-player game))
                                    (player-y (game-player game)))))
    (when (typep map-cell 'item)
      (write map-cell
             :stream :minibuffer)))
  (redraw-screen)
  (process-key (wait-for-key)
               (game-player game)
               (game-map game))
  (clear-screen))

(define-condition exit-game () ())

(define-key-processor #\q ()
  (declare (ignore player map))
  (error 'exit-game))

(defun run-game (game)
  "Runs game."
  (setf (game-map game)
        (load-map-from-file (make-pathname :directory '(:relative "res")
                                           :name "test-map"
                                           :type "isol")))
  (push-object (game-map game) 2 2 (get-object-instance-from-symbol :gun))
  (push-object (game-map game) 3 13 (get-object-instance-from-symbol :rock))
  (push-object (game-map game) 1 9 (get-object-instance-from-symbol :knife))
  (push-object (game-map game) 2 2 (get-object-instance-from-symbol :long-name))
  (catch 'end-game
    (handler-case
        (with-screen (:noecho :nocursor :cbreak)
          (setf *screen-size* (get-screen-size))
          (destructuring-bind (x . y) *screen-size*
            (when (or (< x 40) (< y 40))
              (wprintw-newline nil
                               "To play ISoL you need at least 40 rows and 40 column in your terminal, sorry. To quit press ^C.")
              (redraw-screen)
              (loop))
            (create-new-window :minibuffer
                               0 (- (1- y) +minibuffer-size+)
                               (- (1- x) +info-window-size+) (1+ +minibuffer-size+)
                               :with-box? t)
            (create-new-window :game-window
                               0 0
                               (- (1- x) +info-window-size+) (- (1- y) +minibuffer-size+)
                               :with-box? t)
            (create-new-window :info-window
                               (- (1- x) +info-window-size+) 0
                               +info-window-size+ y
                               :with-box? t))
          (display-message-in-minibuffer "Welcome to ISoL")
          (loop (game-tick game)
                (sleep 1/100)))
      (sb-sys:interactive-interrupt ()
        (throw 'end-game (values)))
      (exit-game ()
        (throw 'end-game (values))))))
