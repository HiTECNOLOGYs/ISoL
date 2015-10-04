(in-package #:isol)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun get-shaders-location ()
    (merge-pathnames #p"src/shaders/src/" (asdf:system-source-directory :isol))))

;;; **************************************************************************
;;;  Shaders
;;; **************************************************************************

(kit.gl.shader:defdict shaders-4.2 (:shader-path #.(get-shaders-location))
  (kit.gl.shader:shader basic-vertex-vao :vertex-shader (:file "basic-vertex-vao.glsl"))
  (kit.gl.shader:shader basic-fragment-vao :fragment-shader (:file "basic-fragment-vao.glsl"))
  (kit.gl.shader:program (:sprite
                          :uniforms ((:view-matrix "view_m"))
                          :attrs ((:vertex 0)
                                  (:uv 1)))
    (:vertex-shader basic-vertex-vao)
    (:fragment-shader basic-fragment-vao)))

;;; **************************************************************************
;;;  Usage
;;; **************************************************************************

(defmacro shader-dict (dict)
  `(kit.gl.shader:dict ,dict))

(defun compile-shader-dict (shader-dict)
  (kit.gl.shader:compile-shader-dictionary shader-dict))

(defun compile-shaders ()
  (mapc #'compile-shader-dict (list (shader-dict shaders-4.2))))

(defun use-program (dict program-name)
  (kit.gl.shader:use-program dict program-name))
