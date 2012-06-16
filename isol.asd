;;;; isol.asd
;;;;
;;;; Author: Mark Fedurin <hitecnologys@gmail.com>.
;;;; Description: Here is system definition.

(asdf:operate 'asdf:load-op :lispbuilder-sdl)

(defpackage #:isol-system
  (:use #:cl
        #:asdf))

(in-package #:isol-system)

(defsystem "isol"
  :description "Just another rogurelike RPG."
  :version "0.1"
  :author "Mark Fedurin <hitecnologys@gmail.com>"
  :license "GPLv3"
  :components
  ((:module "Base"
      :serial t
      :components
      (:file "objects")
      (:file "map")
      (:file "player")
      (:file "game")
      (:file "main"))))