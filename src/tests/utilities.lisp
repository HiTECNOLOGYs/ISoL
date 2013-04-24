(in-package #:isol)

(defconstant +test-map-path+ (make-pathname :directory '(:relative "res")
                                            :name "test-map"
                                            :type "isol"))

(defmacro with-test-map ((map-variable) &body body)
  `(let ((,map-variable (load-map-from-file +test-map-path+)))
     ,@body))

(defmacro with-test-rendered-map ((variable) &body body)
  (let ((map-variable (gensym)))
    `(with-test-map (,map-variable)
       (let ((,variable (render-map ,map-variable)))
         ,@body))))

(defmacro with-test-screen (&body body)
  `(unwind-protect
        (progn (initialize-screen)
               ,@body)
     (deinitialize-screen)))
