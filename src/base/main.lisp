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
