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
   (vao :initarg :vao)))

(defun copy-image-to-foreign-memory (pointer image)
  (with-slots (width height channels data) image
    (opticl:do-pixels (i j) data
      (multiple-value-bind (r g b a) (opticl:pixel data (- width i 1) (- height j 1))
        (setf (cffi:mem-aref pointer :unsigned-char (* 4 (+ (* j width) i))) r
              (cffi:mem-aref pointer :unsigned-char (+ 1 (* 4 (+ (* j width) i)))) g
              (cffi:mem-aref pointer :unsigned-char (+ 2 (* 4 (+ (* j width) i)))) b
              (cffi:mem-aref pointer :unsigned-char (+ 3(* 4 (+ (* j width) i)))) a)))))

(defun make-gl-texture (target mipmap-level image &key border?)
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
      texture)))

(defun make-texture (target mipmap-level image &key border?)
  (make-instance 'Texture
                 :pointer (make-gl-texture target mipmap-level image :border? border?)
                 :vao (make-vao #(0.0 0.0 0.0 1.0
                                  1.0 0.0 0.0 0.0
                                  1.0 1.0 1.0 0.0
                                  0.0 1.0 1.0 1.0))))

(defgeneric enable-texture (target texture))

(defmethod enable-texture (target (texture Texture))
  (with-slots (pointer) texture
    (gl:bind-texture target pointer)))

;;; **************************************************************************
;;;  Texture atlases
;;; **************************************************************************

;;; TODO Make atlases support textures with unequal frames
(defclass Texture-atlas (Texture)
  ((n-frames-x :initarg :n-frames-x)
   (n-frames-y :initarg :n-frames-y)
   (current-frame-x :initarg :current-frame-x
                    :initform 0)
   (current-frame-y :initarg :current-frame-y
                    :initform 0)
   (frame-size-x)
   (frame-size-y)
   (all-vaos)))

(defun calculate-atlas-rects (atlas)
  (with-slots (frame-size-x frame-size-y n-frames-x n-frames-y) atlas
    (loop
      for i below n-frames-x
      appending (loop
                   for j below n-frames-y
                   collecting (vector 0.0 0.0
                                      (* frame-size-x i) (* frame-size-y (1+ j))
                                      1.0 0.0
                                      (* frame-size-x i) (* frame-size-y j)
                                      1.0 1.0
                                      (* frame-size-x (1+ i)) (* frame-size-y j)
                                      0.0 1.0
                                      (* frame-size-x (1+ i)) (* frame-size-y (1+ j)))))))

(defun get-atlas-current-rect (atlas)
  (with-slots (n-frames-y current-frame-x current-frame-y all-vaos) atlas
    (nth (+ (* current-frame-y n-frames-y) current-frame-x) all-vaos)))

(defmethod initialize-instance :after ((instance Texture-atlas) &key &allow-other-keys)
  (with-slots (n-frames-x n-frames-y frame-size-x frame-size-y vao all-vaos) instance
    (setf frame-size-x (/ 1.0 n-frames-x)
          frame-size-y (/ 1.0 n-frames-y)
          all-vaos     (apply #'make-vaos (calculate-atlas-rects instance))
          vao          (get-atlas-current-rect instance))))

(defgeneric switch-texture-frame (texture new-frame-x new-frame-y))

(defmethod switch-texture-frame ((texture Texture-atlas) new-frame-x new-frame-y)
  (with-slots (current-frame-x current-frame-y vao) texture
    (setf current-frame-x new-frame-x
          current-frame-y new-frame-y
          vao (get-atlas-current-rect texture))))

(defun make-texture-atlas (size-x size-y target mipmap-level image &key border?)
  (make-instance 'Texture-atlas
                 :pointer (make-gl-texture target mipmap-level image :border? border?)
                 :n-frames-x size-x
                 :n-frames-y size-y))

;;; **************************************************************************
;;;  Animations
;;; **************************************************************************

(defclass Animated-texture (Texture-atlas)
  ((frame-rate :initarg :frame-rate)
   (counter :initform 0)
   (sequence :initarg :sequence)
   (current-frame :initform 0)))

(defgeneric next-frame (textute))
(defgeneric animation-tick (texture))

(defmethod next-frame ((texture Animated-texture))
   (with-slots (sequence current-frame current-frame-x current-frame-y) texture
     (mod-incf current-frame (length sequence))
     (destructuring-bind (x y) (elt sequence current-frame)
       (setf current-frame-x x
             current-frame-y y))
     (switch-texture-frame texture current-frame-x current-frame-y)))

(defmethod animation-tick ((texture Animated-texture))
  (with-slots (frame-rate counter) texture
    (incf counter)
    (when (<= counter frame-rate)
      (setf counter 0)
      (next-frame texture))))

(defun make-animated-texture (size-x size-y sequence frame-rate target mipmap-level image &key border?)
  (make-instance 'Animated-texture
                 :pointer (make-gl-texture target mipmap-level image :border? border?)
                 :n-frames-x size-x
                 :n-frames-y size-y
                 :sequence sequence
                 :frame-rate frame-rate))
