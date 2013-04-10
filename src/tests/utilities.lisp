(in-package #:isol)

(defconstant +test-objects-list-path+ (make-pathname :directory '(:relative "res")
                                                     :name "test-objects-list"
                                                     :type "isol"))
(defconstant +test-map-path+          (make-pathname :directory '(:relative "res")
                                                     :name "test-map"
                                                     :type "isol"))

(defmacro with-test-objects-list ((variable) &body body)
  "Reads test objects file and binds the resilt to `variable'"
  `(let ((,variable (load-objects-list-from-file +test-objects-list-path+)))
     ,@body))

(defmacro with-test-map ((map-variable &optional (objects-variable (gensym))) &body body)
  `(with-test-objects-list (,objects-variable)
     (let ((,map-variable (load-map-from-file +test-map-path+ ,objects-variable)))
       ,@body)))

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
