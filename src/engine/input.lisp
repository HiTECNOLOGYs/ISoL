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
;;;  Keys
;;; **************************************************************************

(defun get-char-symbol (char)
  (make-keyword (string (char-upcase char))))

(defun get-symbol-letter (symbol)
  (symbol-name symbol))

(defun get-letter-scancode (letter)
  (sdl2-ffi.functions:sdl-get-scancode-from-name letter))

(defun key-scancode (symbol)
  (get-letter-scancode (get-symbol-letter symbol)))

;;; **************************************************************************
;;;  Keyboard
;;; **************************************************************************

;; ----------------
;; Modes

(defclass input-mode ()
  ((id :initform (error "Input mode must be assigned ID")
       :initarg :id
       :reader input-mode-id)
   (keys :initform (make-instance 'tmap :pred 'fixnum<)
         :initarg :keys
         :accessor input-mode-keys))
  (:documentation "Holds key bindings for specific input mode."))

(defvar *root-input-mode* (make-instance 'input-mode :id :root))

(defgeneric key-binding (mode key)
  (:documentation "Returns function binded to given key under given mode."))

(defgeneric (setf key-binding) (new-value mode key)
  (:documentation "SETF-function for KEY-BINDING."))

(defgeneric bind-key (mode key function)
  (:documentation "Binds given character to some function."))

(defgeneric handle-key (mode key data)
  (:documentation "Runs function binded to some `key' with given arguments."))

(defgeneric copy-input-mode (class mode)
  (:documentation "Copies given input mode producing new instance of given class."))

;; ----------------
;; Methods

(defmethod key-binding ((mode input-mode) (key fixnum))
  (get-gmap (input-mode-keys mode) key))

(defmethod key-binding ((mode input-mode) (key character))
  (key-binding mode (get-letter-scancode (string key))))

(defmethod key-binding ((mode input-mode) (key symbol))
  (key-binding mode (key-scancode key)))

(defmethod (setf key-binding) (new-value (mode input-mode) (key fixnum))
  (set-gmap (input-mode-keys mode) key new-value))

(defmethod (setf key-binding) (new-value (mode input-mode) (key character))
  (setf (key-binding mode (get-letter-scancode (string key))) new-value))

(defmethod (setf key-binding) (new-value (mode input-mode) (key symbol))
  (setf (key-binding mode (key-scancode key)) new-value))

(defmethod bind-key ((mode input-mode) (key fixnum) function)
  (setf (key-binding mode key) function))

(defmethod bind-key ((mode input-mode) (key character) function)
  (setf (key-binding mode (get-letter-scancode (string key))) function))

(defmethod bind-key ((mode input-mode) (key symbol) function)
  (setf (key-binding mode (key-scancode key)) function))

(defmethod copy-input-mode (class (mode input-mode))
  (with-slots (keys id) mode
    (make-instance class :id id
                         :keys (copy-gmap keys))))

(defmethod handle-key ((mode input-mode) (key fixnum) data)
  (when-let (key (key-binding mode key))
    (funcall key data)))

(defmethod handle-key ((mode input-mode) (key symbol) data)
  (handle-key mode (key-scancode key) data))

(defmethod handle-key ((mode input-mode) (key character) data)
  (handle-key mode (get-char-symbol key) data))

;; ----------------
;; Macros

(defmacro with-input-mode (mode &body body)
  "Executes `body' with specific input mode enabled."
  (with-gensyms (context-gensym)
    `(let ((,context-gensym (copy-context *context* :input-mode ,mode)))
       (with-context (,context-gensym)
         ,@body))))

(defun make-input-mode (parent &optional class)
  (copy-input-mode (or class (class-of parent)) parent))

(defun extend-input-mode (mode new-bindings)
  (loop for (key binding) in new-bindings
        doing (bind-key mode key binding))
  mode)

(defmacro input-mode ((&optional (parent-mode '*root-input-mode*) class) &body bindings)
  `(extend-input-mode (make-input-mode ,parent-mode ,class) ',bindings))

;; ----------------
;; SDL macros

(defmacro define-window-keyboard-handler (name &body body)
  `(defmethod kit.sdl2:keyboard-event ((window ,name) state timestamp repeat-p keysym)
     ,@body))

;;; **************************************************************************
;;;  Mouse
;;; **************************************************************************

(defvar *screen-regions* (make-hash-table :test 'equal))

;; ----------------
;; Screen regions handling

(defun screen-region-binding (x1 y1 x2 y2)
  (gethash (list x1 y1 x2 y2) *screen-regions*))

(defun (setf screen-region-binding) (function x1 y1 x2 y2)
  (setf (gethash (list x1 y1 x2 y2) *screen-regions*) function))

(defun bind-screen-region (x1 y1 x2 y2 function)
  (setf (screen-region-binding x1 y1 x2 y2) function))

;; ----------------
;; Macros

(defmacro define-window-region-binding ((x1 y1 x2 y2) function)
  `(bind-screen-region ,x1 ,y1 ,x2 ,y2 ,function))

;; ----------------
;; SDL macros

(defmacro define-window-mouse-button-handler (name &body body)
  `(defmethod kit.sdl2:mousebutton-event ((window ,name) state timestamp button x y)
     ,@body))

(defmacro define-window-mouse-motion-handler (name &body body)
  `(defmethod kit.sdl2:mousemotion-event ((window ,name) timestamp x y)
     ,@body))
