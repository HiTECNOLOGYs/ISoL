;;; Copyright (C) Mark Fedurin, 2011-2014.
;;;
;;; This file is part of ISoL.
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

(in-package :isol)

;;; **************************************************************************
;;;  Base containers frames which also represent base scenes
;;; **************************************************************************

(cl-tui:define-frame game-scene (cl-tui:container-frame))

(cl-tui:define-frame inventory-menu (cl-tui:tabbed-frame))

(cl-tui:define-frame player-menu (cl-tui:tabbed-frame))

(cl-tui:define-frame main-menu (cl-tui:container-frame))

;;; **************************************************************************
;;;  Game scene
;;; **************************************************************************

;; ----------------
;; Map and log

(cl-tui:define-frame game-info (cl-tui:container-frame
                                 :split-type :horizontal)
                     :on game-scene
                     :w 40)


(cl-tui:define-frame game-log (cl-tui:callback-frame
                                :render 'game-log-callback)
                     :on game-info)

(cl-tui:define-frame player-info (cl-tui:callback-frame
                                   :render 'player-info-callback)
                     :on game-info)

(cl-tui:define-frame game-process (cl-tui:container-frame
                                    :split-type :horizontal)
                     :on game-scene)

(cl-tui:define-frame minibuffer (cl-tui:retained-frame)
                     :on game-process
                     :h 1)

(cl-tui:define-frame game-map (cl-tui:callback-frame
                                :render 'game-map-callback)
                     :on game-process)

;;; **************************************************************************
;;;  Inventory menu
;;; **************************************************************************

(cl-tui:define-frame items (cl-tui:callback-frame
                             :render 'items-callback)
                     :on inventory-menu)

;;; **************************************************************************
;;;  Player menu
;;; **************************************************************************


;;; **************************************************************************
;;;  Main menu
;;; **************************************************************************
