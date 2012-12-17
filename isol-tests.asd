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
                                              :components ((:file "packages")
                                                           (:file "suites"
                                                                  :depends-on ("packages"))
                                                           (:file "objects-tests"
                                                                  :depends-on ("packages"
                                                                               "suites"))
                                                           (:file "map-tests"
                                                                  :depends-on ("packages"
                                                                               "suites"))
                                                           (:file "player-tests"
                                                                  :depends-on ("packages"
                                                                               "suites"))
                                                           (:file "game-tests"
                                                                  :depends-on ("packages"
                                                                               "suites"))))))))
