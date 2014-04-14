(defsystem :isol
  :description "Just another roguelike RPG."
  :author "Mark Fedurin <hitecnologys@gmail.com>"
  :license "GPL v3"
  :depends-on (:fiveam
               :cl-charms
               :cl-store
               :alexandria
               :anaphora
               :iterate)
  :in-order-to ((test-op (load-op :isol/tests)))
  :perform (test-op (op component)
             (asdf/package:symbol-call :fiveam :run! :isol))
  :pathname "src/"
  :serial t
  :components ((:file "packages")
               (:file "utilities")
               (:module base
                :serial t
                :components ((:file "utilities")
                             (:file "objects")
                             (:file "items")
                             (:file "map")
                             (:file "keyboard")
                             (:file "player")
                             (:file "windows") ; NOT OPERATING SYSTEM
                             (:file "graphics")
                             (:file "user-interaction")
                             (:file "game")
                             (:file "scenes")
                             (:file "main")))))

(defsystem :isol/tests
  :description "Set of unit-tests for ISoL."
  :author "Mark Fedurin <hitecnologys@gmail.com>"
  :license "GPL v3"
  :depends-on (:fiveam :isol)
  :pathname "src/"
  :serial t
  :components ((:file "packages")
               (:module tests
                :components ((:file "suites")
                             (:file "utilities")
                             (:file "objects-tests"
                              :depends-on ("utilities"
                                           "suites"))
                             (:file "map-tests"
                              :depends-on ("utilities"
                                           "suites"))
                             (:file "player-tests"
                              :depends-on ("utilities"
                                           "suites"))
                             (:file "graphics-tests"
                              :depends-on ("utilities"
                                           "suites"))
                             (:file "game-tests"
                              :depends-on ("utilities"
                                           "suites"))))))
