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
                                              :components ((:file "suites")
                                                           (:file "utilities")
                                                           (:file "objects-tests"
                                                                  :depends-on ("utilities" "suites"))
                                                           (:file "map-tests"
                                                                  :depends-on ("utilities" "suites"))
                                                           (:file "player-tests"
                                                                  :depends-on ("utilities" "suites"))
                                                           (:file "graphics-tests"
                                                                  :depends-on ("utilities" "suites"))
                                                           (:file "game-tests"
                                                                  :depends-on ("utilities" "suites"))))))))

(defmethod perform ((op test-op) (c (eql (find-system :isol))))
  (operate 'load-op :isol)
  (operate 'load-op :isol-tests)
  (mapcar #'fiveam:run '(isol::game-tests
                         isol::graphics-tests
                         isol::map-tests
                         isol::objects-tests
                         isol::player-tests)))
