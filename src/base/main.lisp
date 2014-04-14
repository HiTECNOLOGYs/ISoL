(in-package :isol)

(defparameter *game* (make-game)
  "Stores current game instance.")

(define-condition exit-game () ())

(define-key-processor #\q ()
  (declare (ignore player map))
  (error 'exit-game))

(defun run-game (game)
  "Runs game."
  (push-scene (make-scene (curry #'game-scene game)) game)
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
            (when (or (< x 80) (< y 40))
              (wprintw-newline nil "To play ISoL you need at least 40 rows and 80 column in your terminal, sorry. To quit press ^C.")
              (redraw-screen)
              (loop))
            (remove-all-windows)
            (make-new-window :minibuffer
                             0 (- (1- y) +minibuffer-size+)
                             (- (1- x) +info-window-size+) (1+ +minibuffer-size+)
                             :have-box? t)
            (make-new-window :game-window
                             0 0
                             (- (1- x) +info-window-size+) (- (1- y) +minibuffer-size+)
                             :have-box? t)
            (make-new-window :info-window
                             (- (1- x) +info-window-size+) 0
                             +info-window-size+ y
                             :have-box? t))
          (clear-screen)
          (display-message-in-minibuffer "Welcome to ISoL")
          (loop (game-tick game)
                (sleep 1/100)))
      (sb-sys:interactive-interrupt ()
        (throw 'end-game (values)))
      (exit-game ()
        (throw 'end-game (values))))))

(defun main ()
  "Start point."
  (setf *game* (make-game))
  (run-game *game*))
