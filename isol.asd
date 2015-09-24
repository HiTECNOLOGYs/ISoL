;;; Copyright (C) Mark Fedurin, 2011-2015.
;;; ;;; This file is part of ISoL.
;;;
;;; ISoL is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; ISoL is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with ISoL.  If not, see <http://www.gnu.org/licenses/>.

(defsystem :isol
  :description "Just another roguelike RPG."
  :author "Mark Fedurin <hitecnologys@gmail.com>"
  :license "GPL v3"
  :version (:read-file-form "version.lisp-expr")
  :depends-on (:sdl2
               :sdl2kit
               :cl-opengl
               :opticl
               :cl-cairo2
               :cl-store
               :alexandria
               :iterate
               :anaphora
               :let-plus)
  :in-order-to ((test-op (load-op :isol/tests)))
  :perform (test-op (op component)
             (asdf/package:symbol-call :fiveam :run! :isol.tests))
  :pathname "src/"
  :serial t
  :components ((:file "packages")
               (:file "utilities")
               (:module "engine"
                :components ((:file "contexts")
                             (:file "resources")
                             (:file "graphics"
                              :depends-on ("contexts"
                                           "resources"))
                             (:file "input"
                              :depends-on ("contexts"))
                             (:file "ui"
                              :depends-on ("graphics"
                                           "input"))
                             (:file "scenes"
                              :depends-on ("ui"))
                             (:file "entities"
                              :depends-on ("contexts"))
                             (:file "items"
                              :depends-on ("entities"))
                             (:file "creatures"
                              :depends-on ("items"))
                             (:file "map"
                              :depends-on ("creatures"))
                             (:file "items-generation"
                              :depends-on ("map"))
                             (:file "creatures-generation"
                              :depends-on ("items-generation"))
                             (:file "map-generation"
                              :depends-on ("creatures-generation"))
                             (:file "world"
                              :depends-on ("map-generation"))
                             (:file "game"
                              :depends-on ("scenes"
                                           "world"))))
               ;; ----------------
               #+nil
               (:module "game"
                :serial t
                :components ((:file "world")
                             (:file "characters")
                             (:file "scenes")
                             (:file "controls")))
               ;; ----------------
               #+nil
               (:file "isol")))

(defsystem :isol/tests
  :description "Set of unit-tests for ISoL."
  :author "Mark Fedurin <hitecnologys@gmail.com>"
  :license "GPL v3"
  :version (:read-file-form "version.lisp-expr")
  :depends-on (:fiveam
               :isol)
  :pathname "tests/"
  :serial t
  :components ((:file "packages")
               (:file "suites")
               (:file "utilities")
               (:file "contexts-tests"
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
                             "suites"))))
