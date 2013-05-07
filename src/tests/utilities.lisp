(in-package :isol)

(define-constant +test-map-path+ (make-pathname :directory '(:relative "res")
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
