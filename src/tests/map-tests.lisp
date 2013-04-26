(in-package #:isol)
(in-suite map-tests)

(defmacro check-cell (array x y value)
  `(is (eq (aref ,array ,y ,x) ,value)))

(test test-symbol-conversion
  (let ((instance (get-object-instance-from-symbol :wall)))
    (is (equal (name instance)
               "Wall"))
    (is (equal (description instance)
               "Just rusty old stone wall."))
    (is (= (hp instance)
               10000))
    (is (eql (material instance)
             'stone))))

(test test-objects-manipulations
  (with-test-map (map)
    (is (push-object map 1 1 (get-object-instance-from-symbol :gun)))
    (is (and (eql (class-of (get-map-cell-top map 1 1))
                  (class-of (make-instance 'Weapon)))
             (equal (name (get-map-cell-top map 1 1))
                    "Revolver")))
    (is (push-object map 1 2 (get-object-instance-from-symbol :rock)))
    (is (and (eql (class-of (get-map-cell-top map 1 2))
                  (class-of (make-instance 'Map-Object)))
             (equal (name (get-map-cell-top map 1 2))
                    "Rock")))
    (is (equal (name (pop-object map 1 1))
               "Revolver"))
    (is (equal (name (pop-object map 1 2))
               "Rock"))))

(test (test-map-loading :depends-on test-symbol-conversion)
  (with-test-map (map)
    (is       (map-cell-passable-p map 1 1))
    (is-false (map-cell-passable-p map 0 0))))

(test (test-map-rendering :depends-on test-map-loading)
  (with-test-rendered-map (rendered-map)
    (is (eq (first (first rendered-map)) #\#))
    (is (eq (second (second rendered-map)) #\.))))

