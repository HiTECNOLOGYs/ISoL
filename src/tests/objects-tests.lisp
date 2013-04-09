(in-package #:isol)
(in-suite objects-tests)

(test test-object-type-parsing
  (let ((object (funcall (cdr (parse-object-type '(test "Hello!" #\L nil))))))
    (is (eq (object-display-character object)
            #\L))
    (is (eq (object-passable object)
            nil))))

(test (test-object-types-list-loading :depends-on test-object-type-parsing)
  (let ((objects-list (load-objects-list-from-file "res/test-objects-list.isol")))
    (is (= (length objects-list) 2))))
