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
;;;  Graphics initialization and deinitialization
;;; **************************************************************************

(defun init-graphics ()
  (sdl2.kit:start))

(defun deinit-graphics ()
  (sdl2.kit:quit))

;;; **************************************************************************
;;;  Windows
;;; **************************************************************************

(defclass Window (sdl2.kit:window)
  ((size-x :initarg :size-x)
   (size-y :initarg :size-y)))

(defun display-window (window)
  (sdl2.kit:render window))

(defun make-window (class &rest arguments)
  (apply #'make-instance class arguments))

(defmacro define-window (name (&rest additional-parents) &body slots)
  `(defclass ,name (Window ,@additional-parents)
     (,@slots)))

(defmacro define-window-init (name &body body)
  `(defmethod initialize-instance ((window ,name) &key &allow-other-keys)
     (call-next-method)
     ,@body))

(defmacro define-window-render (name &body body)
  `(defmethod sdl2.kit:render ((window ,name))
     ,@body))

(defmacro define-window-close (name &body body)
  `(defmethod sdl2.kit:close-window ((window ,name))
     ,@body))

;;; TODO Move FPS somewhere where I can tweak it easily.
(defmethod initialize-instance :after ((window Window) &key &allow-other-keys)
  (setf (idle-render display) t)
  ;; SDL
  (sdl2:gl-set-swap-interval 60) ; FPS
  ;; OpenGL
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (gl:viewport 0 0 (width display) (height display))
  (with-slots (size-x size-y) window
    (glu:ortho-2d 0 size-x 0 size-y))
  (gl:matrix-mode :modelview)
  (gl:load-identity)
  (gl:enable :texture-2d
             :blend
             :multisample)
  (gl:hint :texture-compression-hint :nicest)
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  (gl:clear-color 0.0 0.0 0.0 1.0))


