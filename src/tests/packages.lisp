(defpackage #:isol.tests/suites
  (:use #:cl
        #:fiveam)
  (:export #:objects-tests
           #:map-tests
           #:player-tests
           #:game-tests))

(defpackage #:isol.tests/objects
  (:use #:cl
        #:isol.tests/suites
        #:isol.objects
        #:fiveam)
  (:export #:test-object-type-parsing
           #:test-object-types-list-loading))

(defpackage #:isol.tests/map
  (:use #:cl
        #:isol.tests/suites
        #:isol.objects
        #:isol.map
        #:fiveam))

(defpackage #:isol.tests/player
  (:use #:cl
        #:isol.tests/suites
        #:fiveam))

(defpackage #:isol.tests/game
  (:use #:cl
        #:isol.tests/suites
        #:fiveam))
