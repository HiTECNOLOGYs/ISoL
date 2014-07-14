(in-package :isol)

;;; *********************************************************************
;;; Scenes
;;; *********************************************************************

(defclass Scene ()
  ((windows :initarg windows
            :initform nil
            :accessor scene-windows)
   (keybindings :initarg :keybindings
                :accessor scene-keybindings)
   (variables :initarg :variables
              :accessor scene-variables)
   (function :initarg :function
             :accessor scene-function))
  (:documentation "Stores information about current scene."))

(defun make-scene (function
                   &key (windows nil windows-given?)
                        (keybindings nil keybindings-given?)
                        (variables nil variables-given?))
  (let ((scene (make-instance 'Scene :function function)))
    (when windows-given?
      (setf (scene-windows scene) windows))
    (when keybindings-given?
      (setf (scene-keybindings scene) keybindings))
    (when variables-given?
      (setf (scene-variables scene) variables))
    scene))

(defgeneric game-current-scene (object))
(defmethod game-current-scene ((game Game))
  (car (game-scenes game)))

(defgeneric push-scene (scene object))
(defmethod push-scene ((scene Scene) (game Game))
  (push scene (game-scenes game)))

(defgeneric pop-scene (object))
(defmethod pop-scene ((game Game))
  (pop (game-scenes game)))

(defgeneric run-scene (object))
(defmethod run-scene ((scene Scene))
  (when (slot-boundp scene 'function)
    (funcall (scene-function scene))))

(defgeneric game-tick (object))
(defmethod game-tick ((game Game))
  (run-scene (game-current-scene game)))

;;; *********************************************************************
;;; Scenes dispatchers
;;; *********************************************************************

(defun game-scene (game)
  "Game step. Draws map, PC, stuff and prompts player for action."
  (with-slots (display-character location) (game-player game)
    (destructuring-bind (x y) location
      #+nil
      (let ((map-cell (get-map-cell-top (game-map game) x y)))
        (when (typep map-cell 'Item)
          (put-text 'minibuffer 1 1 map-cell)))
      (put-char 'game-map x y display-character)))
  (redraw-screen)
  (process-key (wait-for-key) game))

(defun menu-scene ()
  ;; (display-center-menu )
  )
