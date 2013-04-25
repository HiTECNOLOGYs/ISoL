(in-package #:isol)
(in-suite player-tests)

(defun gen-integer (&key (max (1+ most-positive-fixnum)) (min (1- most-negative-fixnum)))
  (+ min (random (1+ (- max min)))))

(test test-player-location
  (let* ((x (gen-integer))
         (y (gen-integer))
         (player (make-instance 'player 
                                :location (list x y))))
    (is (= (first (location player)) x))
    (is (= (second (location player)) y))))

(test test-keybindigns
  (let ((some-random-integer (gen-integer))
        (some-random-char (code-char (gen-integer :min 20 :max 128))))
    (define-key-processor some-random-char (some-number)
      (declare (ignore player map))
      (is (= some-number some-random-integer)))
    (process-key some-random-char nil nil some-random-integer)
    (clear-key-bindings)))
