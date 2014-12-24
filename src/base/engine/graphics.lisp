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

(defclass Window (sdl2.kit:gl-window)
  ((size-x :initarg :size-x)
   (size-y :initarg :size-y)))

(defun make-window (class &rest arguments)
  (apply #'make-instance class arguments))

(defmacro define-window (name (&rest additional-parents) &body slots)
  `(defclass ,name (Window ,@additional-parents)
     (,@slots)))

(defmacro define-window-init (name &body body)
  `(defmethod initialize-instance :after ((window ,name) &key &allow-other-keys)
     ,@body))

(defmacro define-window-render (name &body body)
  `(defmethod sdl2.kit:render ((window ,name))
     ,@body))

(defmacro define-window-close (name &body body)
  `(defmethod sdl2.kit:close-window ((window ,name))
     ,@body))

;;; TODO Move FPS somewhere where I can tweak it easily.
(defmethod initialize-instance ((window Window) &key &allow-other-keys)
  (call-next-method)
  (setf (sdl2.kit:idle-render window) t)
  ;; OpenGL
  ;(with-slots (size-x size-y) window
  ;  (gl:viewport 0 0 size-x size-y))
  (gl:viewport 0 0 640 480)
  (gl:matrix-mode :projection)
  (gl:ortho -2 2 -2 2 -2 2)
  (gl:matrix-mode :modelview)
  (gl:enable :texture-2d
             :blend)
  (gl:disable :multisample
              :multisample-arb)
  (gl:hint :texture-compression-hint :nicest)
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  (gl:load-identity))

(defmethod sdl2.kit:close-window ((window Window))
  (call-next-method))

;;; **************************************************************************
;;;  VBOs
;;; **************************************************************************

(defclass VBO ()
  ((pointer :initarg :pointer)))

(defun gen-buffers-arb (count)
  (cffi:with-foreign-object (buffer-array '%gl:uint count)
    (%gl:gen-buffers-arb count buffer-array)
    (loop for i below count
          collecting (cffi:mem-aref buffer-array '%gl:uint))))

(defun make-vbo (target size data usage)
  (let ((buffer-arb (first (gen-buffers-arb 1))))
    (%gl:bind-buffer-arb target buffer-arb)
    (with-foreign-vector (buffer '%gl:uint data)
      (%gl:buffer-data-arb target size buffer usage))
    (make-instance 'VBO :pointer buffer-arb)))

(defgeneric enable-vbo (target vbo))

(defmethod enable-vbo (target (vbo VBO))
  (with-slots (pointer) vbo
    (%gl:bind-buffer-arb target pointer)))

;;; **************************************************************************
;;;  Textures
;;; **************************************************************************

(defclass Texture ()
  ((pointer :initarg :pointer)))

(defun copy-image-to-foreign-memory (pointer image)
  (with-slots (width height channels data) image
    (opticl:do-pixels (i j) data
      (multiple-value-bind (r g b a) (opticl:pixel data i j)
        (setf (cffi:mem-aref pointer :unsigned-char (* 4 (+ (* j width) i))) r
              (cffi:mem-aref pointer :unsigned-char (+ 1 (* 4 (+ (* j width) i)))) g
              (cffi:mem-aref pointer :unsigned-char (+ 2 (* 4 (+ (* j width) i)))) b
              (cffi:mem-aref pointer :unsigned-char (+ 3(* 4 (+ (* j width) i)))) a)))))

(defun make-texture (target mipmap-level image &key border?)
  (with-slots (width height channels format data) image
    (let ((texture (first (gl:gen-textures 1))))
      (gl:bind-texture target texture)
      (gl:tex-parameter target :generate-mipmap t)
      (gl:tex-parameter target :texture-max-anisotropy-ext 16)
      (gl:tex-parameter target :texture-min-filter :linear-mipmap-linear)
      (cffi:with-foreign-object (pointer :unsigned-char (* width height channels))
        (copy-image-to-foreign-memory pointer image)
        (gl:tex-image-2d target mipmap-level
                         format width height (if border? 1 0)
                         format :unsigned-byte
                         pointer))
      (make-instance 'Texture :pointer texture))))

(defgeneric enable-texture (target texture))

(defmethod enable-texture (target (texture Texture))
  (with-slots (pointer) texture
    (gl:bind-texture target pointer)))
