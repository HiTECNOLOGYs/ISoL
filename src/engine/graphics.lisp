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

(defclass Window (kit.sdl2:gl-window)
  ((view-matrix)
   (shaders)))

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
  (setf (slot-value window 'view-matrix) (kit.glm:ortho-matrix 0 w 0 h -1 1))
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

(kit.gl.vao:defvao 2d-texture ()
  (:separate ()
   (vertex :float 2)
   (uv :float 2)))

(defun make-vao (type vertex-data uv-data)
  (let* ((float-size 4)
         (vertex-length (length vertex-data))
         (uv-length (length uv-data))
         (vao (make-instance 'kit.gl.vao:vao
                             :type type
                             :primitive :quads
                             :vertex-count (/ vertex-length 2))))
    (kit.gl.vao:vao-buffer-vector vao 0 (* float-size vertex-length) vertex-data :static-draw)
    (kit.gl.vao:vao-buffer-vector vao 1 (* float-size uv-length) uv-data :static-draw)
    vao))

(defun float-vector (&rest args)
  (make-array (length args)
              :element-type 'single-float
              :initial-contents args))

(defun draw-vao (vao)
  (kit.gl.vao:vao-draw vao)
  (kit.gl.vao:vao-unbind))

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
      (setf vao (make-vao '2d-texture
                          (float-vector 0.0           0.0
                                        (+ (float width)) 0.0
                                        (float width) (float height)
                                        0.0           (float height))
                          (float-vector 0.0 0.0
                                        1.0 0.0
                                        1.0 1.0
                                        0.0 1.0))))
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
      (draw-vao vao))))

;;; **************************************************************************
;;;  Texture atlases
;;; **************************************************************************

;;; TODO Make atlases support textures with unequal frames
(defclass Texture-atlas ()
  ((textures)
   (n-frames-x :initarg :n-frames-x)
   (n-frames-y :initarg :n-frames-y)
   (current-frame-x :initarg :current-frame-x
                    :initform 0)
   (current-frame-y :initarg :current-frame-y
                    :initform 0)
   (frame-width)
   (frame-height)
   (frame-size-x)
   (frame-size-y)))

(defun make-atlas-vaos (atlas)
  (with-slots (frame-size-x frame-size-y n-frames-x n-frames-y frame-width frame-height) atlas
    (let ((result (make-array (list n-frames-x n-frames-y))))
      (dotimes (j n-frames-y result)
        (dotimes (i n-frames-x)
          (let ((width frame-width)
                (height frame-height)
                (x-coord (* frame-size-x i))
                (y-coord (* frame-size-y j))
                (1+x-coord (* frame-size-x (1+ i)))
                (1+y-coord (* frame-size-y (1+ j))))
            (let ((vao (make-vao '2d-texture
                                 (float-vector 0.0           0.0
                                               (float width) 0.0
                                               (float width) (float height)
                                               0.0           (float height))
                                 (float-vector x-coord   y-coord
                                               1+x-coord y-coord
                                               1+x-coord 1+y-coord
                                               x-coord   1+y-coord))))
              (setf (aref result i (- n-frames-y j 1)) vao))))))))

(defmethod initialize-instance :after ((instance Texture-atlas) &key image &allow-other-keys)
  (with-slots (n-frames-x n-frames-y frame-size-x frame-size-y textures frame-width frame-height) instance
    (setf frame-size-x    (/ 1.0 n-frames-x)
          frame-size-y    (/ 1.0 n-frames-y)
          frame-width     (/ (slot-value image 'width) n-frames-x)
          frame-height    (/ (slot-value image 'height) n-frames-y))
    (setf textures (make-array (list n-frames-x n-frames-y)))
    (let ((gl-texture (make-gl-texture :texture-2d 0 image))
          (vaos (make-atlas-vaos instance)))
      (doarray (i j textures)
        (setf (aref textures i j) (make-instance 'texture
                                                 :pointer gl-texture
                                                 :mipmap-level 0
                                                 :target :texture-2d
                                                 :image image
                                                 :width frame-width
                                                 :height frame-height
                                                 :vao (aref vaos i j)))))))

(defun get-atlas-frame (atlas x y)
  (aref (slot-value atlas 'textures) x y))

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
  (with-slots (textures current-frame-x current-frame-y) atlas
    (draw (aref textures current-frame-x current-frame-y))))

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
;;;  Transformations
;;; **************************************************************************

(defun matrix-translate (matrix &key (x 0.0) (y 0.0) (z 0.0))
  (kit.glm:matrix* matrix (kit.glm:translate* x y z)))

(defun matrix-scale (matrix &key (x 0.0) (y 0.0) (z 0.0))
  (kit.glm:matrix* matrix (kit.glm:scale* x y z)))

(defun matrix-rotate (matrix &key (x 0.0) (y 0.0) (z 0.0))
  (kit.glm:matrix* matrix (kit.glm:rotate* x y z)))

;;; **************************************************************************
;;;  Sprites
;;; **************************************************************************

(defclass Sprite ()
  ((model-matrix :initarg :model-matrix
                 :initform (kit.glm:identity-matrix))
   (position-x :initarg :position-x
               :accessor sprite-position-x)
   (position-y :initarg :position-y
               :accessor sprite-position-y)
   (scale :initarg :scale
          :accessor sprite-scale)
   (rotation :initarg :rotation
             :accessor sprite-rotation)
   (texture :initarg :texture
            :accessor sprite-texture)))

(defmethod initialize-instance :after ((instance sprite) &rest initargs)
  (declare (ignore initargs))
  (move instance))

(defgeneric move (sprite)
  (:method ((sprite sprite))
   (with-slots (model-matrix position-x position-y scale rotation texture) sprite
     (let ((matrix (kit.glm:identity-matrix)))
       (destructuring-bind (x y) scale
         (setf matrix (matrix-scale matrix :x x :y y :z 1.0)))
       (setf matrix (matrix-translate matrix :x position-x :y position-y :z 0.0))
       (destructuring-bind (x y z) rotation
         (setf matrix (matrix-rotate matrix :x x :y y :z z)))
       (setf model-matrix matrix)))))

(defmethod draw ((sprite Sprite))
  ;; Sprites are mostly rectangles. At least rectangles are easier to work with.
  (draw (sprite-texture sprite)))

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

(defvar *text-atlas*)

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
      with vertices = (make-array (* 2 4 (length text))
                                  :element-type 'single-float
                                  :fill-pointer 0)
      with uvs = (make-array (* 2 4 (length text))
                             :element-type 'single-float
                             :fill-pointer 0)
      for char across text
      for char-position = (- (char-code char) 32)
      for i from 0
      finally (return (make-vao '2d-texture vertices uvs))
      when (<= 0 char-position 94)
        doing
           (let ((x-coord (float (* char-position char-uv-width)))
                 (y-coord 0.0)
                 (1+x-coord (float (* char-uv-width (1+ char-position))))
                 (1+y-coord 1.0)
                 (x-vert (float (* i char-width)))
                 (1+x-vert (float (* (1+ i) char-width))))
             (vector-push* vertices
                           x-vert   0.0
                           1+x-vert 0.0
                           1+x-vert (float height)
                           x-vert   (float height))
             (vector-push* uvs
                           x-coord   y-coord
                           1+x-coord y-coord
                           1+x-coord 1+y-coord
                           x-coord   1+y-coord)))))

(defun update-text (text)
  (with-slots (vao atlas content) text
    (setf vao (make-text-vao content atlas))))

(defmethod initialize-instance :after ((instance Text) &rest initargs)
  (declare (ignore initargs))
  (update-text instance))

(defmethod (setf text-content) (new-value (instance Text))
  (setf (text-content instance) new-value)
  (update-text instance)
  new-value)

(defmethod draw ((text Text))
  (enable-texture :texture-2d (slot-value text 'atlas))
  (draw-vao (slot-value text 'vao)))

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
