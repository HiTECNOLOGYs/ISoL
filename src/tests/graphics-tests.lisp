(in-package :isol)
(in-suite graphics-tests)

(test test-screen-manipulations
  (is (and (initialize-screen :noecho :keypad)
           (not (deinitialize-screen)))))

(test (test-player-printing :depends-on test-screen-manipulations)
  (with-test-screen
    (let ((player (make-instance 'player :location (list 1 2))))
      (write player :stream :game-window))))

(test (test-map-output :depends-on (and test-screen-manipulations test-map-rendering))
  (with-test-map (map)
    (with-test-screen
      (is (print-map map)))))
