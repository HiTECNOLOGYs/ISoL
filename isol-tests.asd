(defpackage #:isol-tests-system
  (:use #:cl
        #:asdf))

(in-package #:isol-tests-system)

(defsystem "isol-tests"
  :description "set of utit-tests for isol"
  :author "Mark Fedurin <hitecnologys@gmail>"
  :license "GPL v3"
  :defsystem-depends-on (isol fiveam)
  :components ((:module src
                        :components ((:module tests
                                              :serial t
                                              :components ((:file "packages")
                                                           (:file "suites")
                                                           (:file "utilities")
                                                           (:file "objects-tests")
                                                           (:file "map-tests")
                                                           (:file "player-tests")
                                                           (:file "graphics-tests")
                                                           (:file "game-tests")))))))
