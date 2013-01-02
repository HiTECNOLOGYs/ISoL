(defpackage #:isol.tests/suites
  (:use #:cl
        #:fiveam)
  (:export #:objects-tests
           #:map-tests
           #:player-tests
           #:graphics-tests
           #:game-tests))

(defpackage #:isol.tests/utilities
  (:use #:cl
        #:isol.objects
        #:isol.map)
  (:export #:with-test-objects-list
           #:with-test-map
           #:with-test-rendered-map))

(defpackage #:isol.tests/objects
  (:use #:cl
        #:isol.tests/suites
        #:isol.objects
        #:isol.tests/utilities
        #:fiveam)
  (:export #:test-object-type-parsing
           #:test-object-types-list-loading))

(defpackage #:isol.tests/map
  (:use #:cl
        #:isol.map
        #:isol.objects
        #:isol.tests/suites
        #:isol.tests/objects
        #:isol.tests/utilities
        #:fiveam)
  (:export #:test-list-size-searching
           #:test-list-to-array-conversion
           #:test-map-loading
           #:test-map-rendering))

(defpackage #:isol.tests/player
  (:use #:cl
        #:isol.player
        #:isol.tests/suites
        #:isol.tests/utilities
        #:fiveam))

(defpackage #:isol.tests/graphics
  (:use #:cl
        #:isol.graphics
        #:isol.tests/suites
        #:isol.tests/objects
        #:isol.tests/map
        #:isol.tests/player
        #:isol.tests/utilities
        #:fiveam))

(defpackage #:isol.tests/game
  (:use #:cl
        #:isol.tests/suites
        #:isol.tests/utilities
        #:fiveam))
