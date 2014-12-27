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
(defmethod initialize-instance :after ((window Window) &key size-x size-y
                                                       &allow-other-keys)
  (setf (sdl2.kit:idle-render window) t)
  ;; OpenGL
  (with-slots ((window-size-x size-x) (window-size-y size-y)) window
    (setf window-size-x size-x
          window-size-y size-y)
    (gl:viewport 0 0 size-x size-y))
  (gl:viewport 0 0 600 600)
  (gl:matrix-mode :projection)
  (gl:ortho -2 2 -2 2 -2 2)
  (gl:matrix-mode :modelview)
  (gl:enable :texture-2d
             :blend)
  (gl:disable :multisample)
  (gl:hint :texture-compression-hint :nicest)
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  (gl:load-identity))

(defmethod sdl2.kit:close-window ((window Window))
  (call-next-method))

;;; **************************************************************************
;;;  VAOs
;;; **************************************************************************

(defclass VAO ()
  ((pointer :initarg :pointer)
   (length :initarg :length)))

(defun make-vao (data)
  (let ((float-size 4)
        (vao (gl:gen-vertex-array))
        (vbo (first (gl:gen-buffers 1))))
    (gl:bind-vertex-array vao)
    (gl:bind-buffer :array-buffer vbo)
    (with-foreign-vector (data-ptr :float data)
      (%gl:buffer-data :array-buffer (* float-size (length data)) data-ptr :static-draw))
    (gl:enable-client-state :vertex-array)
    (%gl:vertex-pointer 2 :float (* 4 float-size) (cffi:make-pointer 0))
    (gl:enable-client-state :texture-coord-array)
    (%gl:tex-coord-pointer 2 :float (* 4 float-size) (cffi:make-pointer (* float-size 2)))
    (gl:bind-vertex-array 0)
    (gl:delete-buffers (list vbo))
    (make-instance 'VAO
                   :pointer vao
                   :length (/ (length data) 4))))

(defun make-vaos (&rest data)
  (loop for dat in data
        collecting (make-vao dat)))

(defgeneric enable-vao (vao))

(defmethod enable-vao ((vao VAO))
  (with-slots (pointer) vao
    (gl:bind-vertex-array pointer)))

;;; **************************************************************************
;;;  Textures
;;; **************************************************************************

(defclass Texture ()
  ((pointer :initarg :pointer)
   (coords :initarg :coords)))

(defun copy-image-to-foreign-memory (pointer image)
  (with-slots (width height channels data) image
    (opticl:do-pixels (i j) data
      (multiple-value-bind (r g b a) (opticl:pixel data (- width i 1) (- height j 1))
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
      (make-instance 'Texture
                     :pointer texture
                     :coords (make-vao #(0.0 0.0 0.0 1.0
                                         1.0 0.0 0.0 0.0
                                         1.0 1.0 1.0 0.0
                                         0.0 1.0 1.0 1.0))))))

(defgeneric enable-texture (target texture))

(defmethod enable-texture (target (texture Texture))
  (with-slots (pointer) texture
    (gl:bind-texture target pointer)))
