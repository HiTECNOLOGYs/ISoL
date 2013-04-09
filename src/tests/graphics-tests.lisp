(in-package #:isol)
(in-suite graphics-tests)

(test test-screen-manipulations
  (is (and (initialize-screen)
           (deinitialize-screen))))


(test (test-map-output :depends-on (and test-screen-manipulations test-map-rendering))
  (with-test-map (map)
    (initialize-screen)
    (is (print-map map))
    (deinitialize-screen)))
