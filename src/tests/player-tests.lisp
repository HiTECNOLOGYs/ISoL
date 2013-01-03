(in-package #:isol.tests/player)
(in-suite player-tests)

(defun gen-integer (&key (max (1+ most-positive-fixnum)) (min (1- most-negative-fixnum)))
  (+ min (random (1+ (- max min)))))

(defparameter +default-list-length+ 10)

(defun gen-list (&key (length (1+ (random +default-list-length+))))
  (unless (zerop length)
    (cons (random (1- most-positive-fixnum))
          (gen-list :length (1- length)))))

(test test-player-position
  (let* ((x (gen-integer))
         (y (gen-integer))
         (player (make-player :position (list x y))))
    (is (= (player-x player) x))
    (is (= (player-y player) y))))

(test test-actions
  (let ((some-random-integer (gen-integer)))
    (defaction test-action (player some-number)
      (declare (ignore player))
      (is (= some-number
             some-random-integer)))
    (run-action nil 'test-action some-random-integer)))

(test test-keybindigns
  (let ((some-random-integer (gen-integer))
        (some-random-list (gen-list)))
    (defbinding test-binding (player some-number) some-random-list
      (declare (ignore player))
      (is (= some-number some-random-integer)))
    (process-keys nil some-random-list some-random-integer)))
