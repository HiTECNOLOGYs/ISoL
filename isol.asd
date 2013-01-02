(defpackage #:isol-system
  (:use #:cl
        #:asdf))

(in-package #:isol-system)

(defsystem "isol"
  :description "Just another roguelike RPG."
  :version "0.1"
  :author "Mark Fedurin <hitecnologys@gmail.com>"
  :license "GPL v3"
  :defsystem-depends-on (cl-ncurses)
  :components ((:module src
                        :components ((:module base
                                              :serial t
                                              :components ((:file "packages")
                                                           (:file "utilities")
                                                           (:file "objects")
                                                           (:file "map")
                                                           (:file "player")
                                                           (:file "graphics")
                                                           (:file "game")
                                                           (:file "main")))))))
