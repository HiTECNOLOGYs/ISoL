(defpackage #:isol.system-definition
  (:use #:cl
        #:asdf))

(in-package #:isol.system-definition)

(defsystem :isol
  :description "Just another roguelike RPG."
  :author "Mark Fedurin <hitecnologys@gmail.com>"
  :license "GPL v3"
  :defsystem-depends-on (#:fiveam #:cl-ncurses)
  :components ((:module src
                        :serial t
                        :components ((:file "packages")
                                     (:module base
                                              :serial t
                                              :components ((:file "utilities")
                                                           (:file "objects")
                                                           (:file "map")
                                                           (:file "player")
                                                           (:file "graphics")
                                                           (:file "game")
                                                           (:file "main")))))))

(defsystem :isol-tests
  :description "Set of unit-tests for ISoL."
  :author "Mark Fedurin <hitecnologys@gmail>"
  :license "GPL v3"
  :defsystem-depends-on (#:fiveam #:isol)
  :components ((:module src
                        :serial t
                        :components ((:file "packages")
                                     (:module tests
                                              :serial t
                                              :components ((:file "suites")
                                                           (:file "utilities")
                                                           (:file "objects-tests")
                                                           (:file "map-tests")
                                                           (:file "player-tests")
                                                           (:file "graphics-tests")
                                                           (:file "game-tests")))))))
