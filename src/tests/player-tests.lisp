(in-package #:isol)
(in-suite player-tests)

(defun gen-integer (&key (max (1+ most-positive-fixnum)) (min (1- most-negative-fixnum)))
  (+ min (random (1+ (- max min)))))

(test test-player-position
  (let* ((x (gen-integer))
         (y (gen-integer))
         (player (make-player :position (list x y))))
    (is (= (player-x player) x))
    (is (= (player-y player) y))))

(defun clean-stuff (function)
  (fmakunbound function)
  (setf *player-actions* nil)
  (setf *keyboard-hooks* nil))

(test test-actions
  (let ((some-random-integer (gen-integer)))
    (defaction test-action (some-number)
      (declare (ignore player))
      (is (= some-number
             some-random-integer)))
    (run-action nil 'test-action some-random-integer)
    (clean-stuff 'test-action)))

(test test-keybindigns
  (let ((some-random-integer (gen-integer))
        (some-random-char (code-char (gen-integer :min 20 :max 128))))
    (defbinding (test-binding some-random-char) (some-number)
      (declare (ignore player))
      (is (= some-number some-random-integer)))
    (process-key nil some-random-char some-random-integer)
    (clean-stuff 'test-binding)))
