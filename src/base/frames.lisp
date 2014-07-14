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

(cl-tui:define-frame game-info (cl-tui:callback-frame
                                 :render 'game-info-callback)
                     :on game-scene
                     :w 40)

(cl-tui:define-frame game-map (cl-tui:callback-frame
                                :render 'game-map-callback)
                     :on game-scene)

;;; **************************************************************************
;;;  Inventory menu
;;; **************************************************************************


;;; **************************************************************************
;;;  Player menu
;;; **************************************************************************


;;; **************************************************************************
;;;  Main menu
;;; **************************************************************************
