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

(cl-tui:define-frame minibuffer (cl-tui:callback-frame
                                  :render 'minibuffer-callback)
                     :on game-process
                     :h 3)

(cl-tui:define-frame game-map (cl-tui:callback-frame
                                :render 'game-map-callback)
                     :on game-process)

;;; **************************************************************************
;;;  Inventory menu
;;; **************************************************************************


;;; **************************************************************************
;;;  Player menu
;;; **************************************************************************


;;; **************************************************************************
;;;  Main menu
;;; **************************************************************************
