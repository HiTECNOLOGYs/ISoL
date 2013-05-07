(in-package :isol)
(in-suite objects-tests)

(test creature-initialization
  (let ((creature (make-instance 'creature
                                 :max-hp 100
                                 :max-wp 101
                                 :max-hunger 102
                                 :max-thirst 103
                                 :max-energy 104)))
    (is (= (hp creature) 100))
    (is (= (wp creature) 101))
    (is (= (hunger creature) 102))
    (is (= (thirst creature) 103))
    (is (= (energy creature) 104))))
