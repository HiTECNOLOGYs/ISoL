(in-package #:isol.tests/map)
(in-suite map-tests)

(test test-list-size-searching
  (let ((list-size (find-list-size '((1 2 3) (4 5 6)))))
    (is (= (first list-size)
           2))
    (is (= (second list-size))
        3)))

(defmacro check-cell (array x y value)
  `(is (eq (aref ,array ,y ,x) ,value)))

(test (test-list-to-array-conversion :depends-on test-list-size-searching)
  (let ((array (list->array '((1 2 3) (4 5 6)))))
    (check-cell array  0 0  1)
    (check-cell array  1 1  5)
    (check-cell array  2 1  6)))

(test (test-map-loading :depends-on (and test-list-to-array-conversion
                                         test-object-types-list-loading))
  (with-test-map (map)
    (is       (check-passability map 1 0))
    (is-false (check-passability map 0 0))))

(test (test-map-rendering :depends-on test-map-loading)
  (with-test-rendered-map (rendered-map)
    (is (eq (first (first rendered-map)) #\O))
    (is (eq (second (first rendered-map)) #\P))))
