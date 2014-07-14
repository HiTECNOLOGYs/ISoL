(in-package :isol)

;;; *********************************************************************
;;; Scenes
;;; *********************************************************************

(defclass Scene ()
  ((frame :initarg frame
          :initform :root
          :accessor scene-frame)
   (keybindings :initarg :keybindings
                :initform *keys*
                :accessor scene-keybindings)
   (variables :initarg :variables
              :initform nil
              :accessor scene-variables)
   (dispatcher :initarg :dispatcher
               :accessor scene-dispatcher))
  (:documentation "Stores information about current scene."))

(defun make-scene (function
                   &key (frame nil frame-given?)
                        (keybindings nil keybindings-given?)
                        (variables nil variables-given?))
  (let ((scene (make-instance 'Scene :dispatcher function)))
    (when frame-given?
      (setf (scene-frame scene) frame))
    (when keybindings-given?
      (setf (scene-keybindings scene)
            (etypecase keybindings
              (list (alist-hash-table keybindings))
              (hash-table keybindings))))
    (when variables-given?
      (let ((additional-variables (mapcar #'first variables))
            (additional-variables-values (mapcar #'second variables)))
        (setf (scene-variables scene)
              (list additional-variables
                    additional-variables-values))))
    scene))

(defgeneric game-current-scene (object)
  (:method ((object Game))
    (car (game-scenes object))))

(defgeneric push-scene (scene object)
  (:method ((scene Scene) (object Game))
    (push scene (game-scenes object))
    (display (scene-frame scene))
    scene))

(defgeneric pop-scene (object)
  (:method ((object Game))
    (pop (game-scenes object))))

(defgeneric run-scene (object)
  (:method ((scene Scene))
    (when (slot-boundp scene 'dispatcher)
      (funcall (scene-dispatcher scene))))
  (:method :around ((scene Scene))
    (with-slots (keybindings variables) scene
      (let ((*keys* keybindings))
        (progv (first variables) (second variables)
          (call-next-method))))))

(defgeneric game-tick (object)
  (:method ((game Game))
    (run-scene (game-current-scene game))))

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
