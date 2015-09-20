;;; Copyright (C) Mark Fedurin, 2011-2015.
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
  (kit.sdl2:start))

(defun deinit-graphics ()
  (kit.sdl2:quit))

;;; **************************************************************************
;;;  Windows
;;; **************************************************************************

(defclass Window (kit.sdl2:gl-window) ())

(defun make-window (class &rest arguments)
  (apply #'make-instance class arguments))

(defmacro define-window (name (&rest additional-parents) &body slots)
  `(defclass ,name (Window ,@additional-parents)
     (,@slots)))

(defmacro define-window-init (name &body body)
  `(defmethod initialize-instance :after ((window ,name) &key &allow-other-keys)
     ,@body))

(defmacro define-window-render (name &body body)
  `(defmethod kit.sdl2:render ((window ,name))
     ,@body))

(defmacro define-window-close (name &body body)
  `(defmethod kit.sdl2:close-window ((window ,name))
     ,@body))

(defmacro define-window-event-handler (name &body body)
  `(defmethod kit.sdl2:window-event ((window ,name) type timestamp data1 data2)
     ,@body))

;;; TODO Move FPS somewhere where I can tweak it easily.
(defmethod initialize-instance :after ((window Window)
                                       &key (w 800) (h 600) &allow-other-keys)
  (setf (kit.sdl2:idle-render window) t)
  ;; OpenGL
  (gl:viewport 0 0 w h)
  (gl:matrix-mode :projection)
  (gl:ortho 0 w 0 h -1 1)
  (gl:matrix-mode :modelview)
  (gl:enable :texture-2d
             :blend)
  (gl:disable :multisample)
  (gl:hint :texture-compression-hint :nicest)
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  (gl:load-identity))

(defmethod kit.sdl2:close-window ((window Window))
  (call-next-method))

;;; **************************************************************************
;;;  VAOs
;;; **************************************************************************

(defclass VAO ()
  ((pointer :initarg :pointer)
   (length :initarg :length)))

(defmethod initialize-instance :after ((instance VAO) &rest initargs)
  (declare (ignore initargs))
  (let ((pointer (slot-value instance 'pointer)))
    (sb-ext:finalize instance #'(lambda () (gl:delete-vertex-arrays (list pointer))))))

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

(defgeneric enable-vao (vao)
  (:method ((vao VAO))
    (with-slots (pointer) vao
      (gl:bind-vertex-array pointer))))

(defun draw-vao (vao mode first count)
  (enable-vao vao)
  (%gl:draw-arrays mode first count))

;;; **************************************************************************
;;;  Textures
;;; **************************************************************************

(defclass Texture ()
  ((pointer :initarg :pointer)
   (vao :initarg :vao)
   (width :initarg :width)
   (height :initarg :height)))

(defun copy-image-to-foreign-memory (pointer image)
  (with-slots (width height channels data) image
    (opticl:do-pixels (j i) data
      (multiple-value-bind (r g b a) (opticl:pixel data (- height j 1) i)
        (let* ((pixel-start-index (* 4 (+ (* j width) i)))
               (r-index pixel-start-index)
               (g-index (+ 1 pixel-start-index))
               (b-index (+ 2 pixel-start-index))
               (a-index (+ 3 pixel-start-index)))
          (setf (cffi:mem-aref pointer :unsigned-char r-index) r
                (cffi:mem-aref pointer :unsigned-char g-index) g
                (cffi:mem-aref pointer :unsigned-char b-index) b
                (cffi:mem-aref pointer :unsigned-char a-index) a))))))

(defun copy-cairo-image-to-foreign-memory (pointer data w h)
  (dotimes (i h)
    (dotimes (j w)
      (let* ((pixel-start-index (* 4 (+ (* i w) j)))
             (pixel-start-index-new (* 4 (+ (* (- h i 1) w) j)))
             (r (cffi:mem-aref data :unsigned-char pixel-start-index))
             (g (cffi:mem-aref data :unsigned-char (+ 1 pixel-start-index)))
             (b (cffi:mem-aref data :unsigned-char (+ 2 pixel-start-index)))
             (a (cffi:mem-aref data :unsigned-char (+ 3 pixel-start-index))))
        (setf (cffi:mem-aref pointer :unsigned-char pixel-start-index-new) r
              (cffi:mem-aref pointer :unsigned-char (+ 1 pixel-start-index-new)) g
              (cffi:mem-aref pointer :unsigned-char (+ 2 pixel-start-index-new)) b
              (cffi:mem-aref pointer :unsigned-char (+ 3 pixel-start-index-new)) a)))))

(defun make-gl-texture (target mipmap-level image &key border?)
  (with-slots (width height channels format data) image
    (let ((texture (first (gl:gen-textures 1))))
      (gl:bind-texture target texture)
      (gl:tex-parameter target :generate-mipmap t)
      (gl:tex-parameter target :texture-min-filter :nearest)
      (gl:tex-parameter target :texture-mag-filter :nearest)
      (cffi:with-foreign-object (pointer :unsigned-char (* width height channels))
        (cond
          ((cffi:pointerp data)
           (copy-cairo-image-to-foreign-memory pointer data width height)
           (gl:tex-image-2d target mipmap-level
                            format width height (if border? 1 0)
                            format :unsigned-byte
                            pointer))
          (t
           (copy-image-to-foreign-memory pointer image)
           (gl:tex-image-2d target mipmap-level
                            format width height (if border? 1 0)
                            format :unsigned-byte
                            pointer))))
      texture)))

(defmethod initialize-instance :after ((instance Texture)
                                       &key target mipmap-level border? image
                                       &allow-other-keys)
  (with-slots (pointer vao width height) instance
    (unless (and (slot-boundp instance 'width) (slot-boundp instance 'height))
      (setf width (slot-value image 'width)
            height (slot-value image 'height)))
    (unless (slot-boundp instance 'vao)
      (setf vao (make-vao (vector 0.0 0.0                      0.0 0.0
                                  (float width) 0.0            1.0 0.0
                                  (float width) (float height) 1.0 1.0
                                  0.0 (float height)           0.0 1.0))))
    (unless (slot-boundp instance 'pointer)
      (setf pointer (make-gl-texture target mipmap-level image :border? border?))))
  (let ((pointer (slot-value instance 'pointer)))
    (sb-ext:finalize instance #'(lambda () (gl:delete-textures (list pointer))))))

(defun make-texture (target mipmap-level image &key border?)
  (make-instance 'Texture
                 :target target
                 :mipmap-level mipmap-level
                 :border? border?
                 :image image))

(defgeneric enable-texture (target texture)
  (:method (target (texture Texture))
    (with-slots (pointer) texture
      (gl:bind-texture target pointer))))

(defgeneric draw (object)
  (:method ((texture Texture))
    (with-slots (vao) texture
      (enable-texture :texture-2d texture)
      (draw-vao vao :quads 0 (slot-value vao 'length)))))

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
   (frame-size-y)))

(defun make-atlas-vao (atlas)
  (with-slots (frame-size-x frame-size-y n-frames-x n-frames-y width height) atlas
    (let ((result (make-array (* 4 4 (* n-frames-x n-frames-y))
                              :element-type 'float
                              :fill-pointer 0)))
      (dotimes (j n-frames-y (make-vao result))
        (dotimes (i n-frames-x)
          (let ((x-coord (* frame-size-x i))
                (y-coord (* frame-size-y j))
                (1+x-coord (* frame-size-x (1+ i)))
                (1+y-coord (* frame-size-y (1+ j))))
            (vector-push* result
                          0.0 0.0                       x-coord y-coord
                          (float width) 0.0             1+x-coord y-coord
                          (float width) (float height)  1+x-coord 1+y-coord
                          0.0 (float height)            x-coord 1+y-coord)))))))

(defmethod initialize-instance :after ((instance Texture-atlas) &key image &allow-other-keys)
  (with-slots (n-frames-x n-frames-y frame-size-x frame-size-y width height vao) instance
    (setf frame-size-x    (/ 1.0 n-frames-x)
          frame-size-y    (/ 1.0 n-frames-y)
          width           (/ (slot-value image 'width) n-frames-x)
          height          (/ (slot-value image 'height) n-frames-y))
    (setf vao (make-atlas-vao instance))))

(defun get-vao-start (atlas)
  (with-slots (n-frames-x current-frame-x current-frame-y) atlas
    (* 4 (+ (* current-frame-y n-frames-x) current-frame-x))))

(defun switch-atlas-frame (texture new-frame-x new-frame-y)
  (with-slots (current-frame-x current-frame-y vao) texture
    (setf current-frame-x new-frame-x
          current-frame-y new-frame-y)))

(defun make-texture-atlas (size-x size-y target mipmap-level image &key border?)
  (make-instance 'Texture-atlas
                 :n-frames-x size-x
                 :n-frames-y size-y
                 :image image
                 :target target
                 :mipmap-level mipmap-level
                 :border? border?))

(defmethod draw ((atlas Texture-atlas))
  (let ((start (get-vao-start atlas)))
    (with-slots (vao) atlas
      (draw-vao vao :quads start 4))))

;;; **************************************************************************
;;;  Animations
;;; **************************************************************************

(defclass Animated-texture (Texture-atlas)
  ((frame-rate :initarg :frame-rate)
   (counter :initform 0)
   (sequence :initarg :sequence)
   (current-frame :initform 0)))

(defun make-animated-texture (size-x size-y sequence frame-rate target mipmap-level image
                              &key border?)
  (make-instance 'Animated-texture
                 :n-frames-x size-x
                 :n-frames-y size-y
                 :sequence sequence
                 :frame-rate frame-rate
                 :image image
                 :target target
                 :mipmap-level mipmap-level
                 :border? border?))

(defgeneric next-frame (textute)
  (:method ((texture Animated-texture))
    (with-slots (sequence current-frame current-frame-x current-frame-y) texture
      (mod-incf current-frame (length sequence))
      (destructuring-bind (x y) (elt sequence current-frame)
        (setf current-frame-x x
              current-frame-y y))
      (switch-atlas-frame texture current-frame-x current-frame-y))))

(defgeneric animation-tick (texture)
  (:method ((texture Animated-texture))
    (with-slots (frame-rate counter) texture
      (incf counter)
      (when (>= counter frame-rate)
        (setf counter 0)
        (next-frame texture)))))

;;; **************************************************************************
;;;  Sprites
;;; **************************************************************************

(defclass Sprite ()
  ((position-x :initarg :position-x
               :accessor sprite-position-x)
   (position-y :initarg :position-y
               :accessor sprite-position-y)
   (scale :initarg :scale
          :accessor sprite-scale)
   (rotation :initarg :rotation
             :accessor sprite-rotation)
   (texture :initarg :texture
            :accessor sprite-texture)))

(defun apply-transformations (sprite)
  (with-slots (position-x position-y scale rotation texture) sprite
    (gl:load-identity)
    (gl:translate position-x position-y 0.0)
    (destructuring-bind (x y) scale
      (gl:scale x y 1.0))
    (destructuring-bind (x y z) rotation
      (gl:rotate x 1.0 0.0 0.0)
      (gl:rotate y 0.0 1.0 0.0)
      (gl:rotate z 0.0 0.0 1.0))))

(defmethod draw ((sprite Sprite))
  ;; Sprites are mostly rectangles. At least rectangles are easier to work with.
  (gl:with-pushed-matrix
    (apply-transformations sprite)
    (draw (sprite-texture sprite))))

(defun make-sprite (texture x y &key (scale (list 1.0 1.0)) (rotation (list 0.0 0.0 0.0)))
  (make-instance 'Sprite
                 :position-x x
                 :position-y y
                 :scale scale
                 :rotation rotation
                 :texture texture))

;;; **************************************************************************
;;;  Text
;;; **************************************************************************

(defclass Text-atlas (Texture)
  ((font :initarg :font
         :initform "Times")
   (size :initarg :size
         :initform nil)
   (color :initarg :color
          :initform (list 0 0 0 255))
   (char-width :initarg :char-width)
   (char-uv-width :initarg :char-uv-width)))

(defclass Text (Sprite)
  ((content :initarg :content
            :accessor text-content)
   (atlas :initarg :atlas
          :initform *text-atlas*)
   (vao :initarg :vao)))

(defun draw-atlas (surface color font size height)
  (let ((context (cairo:create-context surface)))
    (unwind-protect
         (cairo:with-context (context)
           (destructuring-bind (r g b a) color
             (cairo:set-source-rgba r g b a))
           (cairo:select-font-face font :normal :normal)
           (cairo:set-font-size size)
           (cairo:move-to 1 (* 0.75 height)) ; This works on pure magic
           (cairo:show-text (with-output-to-string (stream)
                              (loop for char-code from 32 to 126 doing
                                (format stream "~A" (code-char char-code))))))
      (cairo:destroy context))))

(defun make-text-atlas (&optional (size 12))
  (let* ((color (list 0 0 0 255))
         (font "Anonymous Pro")
         (height (1+ size))
         (n-chars (1+ (- 126 32)))
         (width (* n-chars (1+ (ceiling size 2)))) ; As well as this.
         (surface (cairo:create-image-surface :argb32 width height)))
    (draw-atlas surface color font size height)
    (prog1
        (let* ((data (cairo:image-surface-get-data surface :pointer-only t))
               (image (make-instance 'Image
                                     :format :rgba
                                     :channels 4
                                     :width width
                                     :height height
                                     :data data)))
          (make-instance 'Text-atlas
                         :font font
                         :color color
                         :size size
                         :char-uv-width (/ 1.0 n-chars)
                         :char-width (/ width n-chars)
                         :target :texture-2d
                         :mipmap-level 0
                         :image image))
      (cairo:destroy surface))))

;; TODO Merge this code with MAKE-ATLAS-VAO as they're very similar.
(defun make-text-vao (text atlas)
  (with-slots (char-width char-uv-width height) atlas
    (loop
      with result = (make-array (* 4 4 (length text))
                                :element-type 'float
                                :fill-pointer 0)
      for char across text
      for char-position = (- (char-code char) 32)
      for i from 0
      finally (return (make-vao result))
      when (<= 0 char-position 94)
        doing
           (let ((x-coord (float (* char-position char-uv-width)))
                 (y-coord 0.0)
                 (1+x-coord (float (* char-uv-width (1+ char-position))))
                 (1+y-coord 1.0)
                 (x-vert (float (* i char-width)))
                 (1+x-vert (float (* (1+ i) char-width))))
             (vector-push* result
                           x-vert 0.0              x-coord y-coord
                           1+x-vert 0.0            1+x-coord y-coord
                           1+x-vert (float height) 1+x-coord 1+y-coord
                           x-vert (float height)   x-coord 1+y-coord)))))

(defun update-text-vao (text)
  (with-slots (vao atlas content) text
    (setf vao (make-text-vao content atlas))))

(defmethod initialize-instance :after ((instance Text) &rest initargs)
  (declare (ignore initargs))
  (update-text-vao instance))

(defmethod (setf text-content) (new-value (instance Text))
  (setf (text-content instance) new-value)
  (update-text-vao instance)
  new-value)

(defmethod draw ((text Text))
  (gl:with-pushed-matrix
    (apply-transformations text)
    (with-slots (atlas vao) text
      (enable-texture :texture-2d atlas)
      (draw-vao vao :quads 0 (slot-value vao 'length)))))

(defun make-text (content x y
                  &key (scale (list 1.0 1.0)) (rotation (list 0.0 0.0 0.0))
                  (atlas (make-text-atlas)))
  (make-instance 'Text
                 :content content
                 :position-x x
                 :position-y y
                 :scale scale
                 :rotation rotation
                 :atlas atlas))
