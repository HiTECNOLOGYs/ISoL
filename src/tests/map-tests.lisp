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

(test (test-map-loading :depends-on test-list-to-array-conversion)
  (let* ((objects-list (load-objects-list-from-file "res/test-objects-list.isol"))
         (map (load-map-from-file "res/test-map.isol" objects-list)))
    (is       (check-passability map 1 0))
    (is-false (check-passability map 0 0))))

(test (test-map-rendering :depends-on test-map-loading)
  (let* ((objects-list (load-objects-list-from-file "res/test-objects-list.isol"))
         (rendered-map (render-map (load-map-from-file "res/test-map.isol" objects-list))))
    (is (eq (first (first rendered-map)) #\O))
    (is (eq (second (first rendered-map)) #\P))))
