;;; Copyright (C) Mark Fedurin, 2011-2014.
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
  :depends-on (:cl-tui
               :cl-store
               :alexandria
               :iterate
               :anaphora
               :let+)
  :in-order-to ((test-op (load-op :isol/tests)))
  :perform (test-op (op component)
             (asdf/package:symbol-call :fiveam :run! :isol))
  :pathname "src/"
  :serial t
  :components ((:file "packages")
               (:file "utilities")
               (:module base
                :serial t
                :components ((:module "engine"
                                      :components ((:file "contexts")
                                                   (:file "graphics"
                                                          :depends-on ("contexts"))
                                                   (:file "keyboard"
                                                          :depends-on ("contexts"))
                                                   (:file "ui"
                                                          :depends-on ("graphics"
                                                                       "keyboard"))
                                                   (:file "scenes"
                                                          :depends-on ("ui"))
                                                   (:file "game"
                                                          :depends-on ("scenes"))))
                             ;; ----------------
                             (:module "world"
                                      :serial t
                                      :components ((:file "entities")
                                                   (:file "items")
                                                   (:file "creatures")
                                                   (:file "map")
                                                   (:file "items-genertion")
                                                   (:file "creatures-generation")
                                                   (:file "map-generation")
                                                   (:file "world")))
                             ;; ----------------
                             (:module "game"
                                      :serial t
                                      :components ((:file "characters")
                                                   (:file "scenes")
                                                   (:file "controls")))
                             (:file "isol")))))

(defsystem :isol/tests
  :description "Set of unit-tests for ISoL."
  :author "Mark Fedurin <hitecnologys@gmail.com>"
  :license "GPL v3"
  :version (:read-file-form "version.lisp-expr")
  :depends-on (:fiveam
               :isol)
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
