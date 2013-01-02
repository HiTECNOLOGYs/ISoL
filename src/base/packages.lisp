(defpackage #:isol.utilities
  (:use #:cl)
  (:export #:when-let
           #:curry
           #:stream->list
           #:doarray
           #:2d-array->list
           #:compose
           #:list->string))

(defpackage #:isol.objects
  (:use #:cl
        #:isol.utilities)
  (:export #:make-object
           #:object-display-character
           #:object-passable
           #:parse-object-type
           #:load-objects-list-from-file
           #:symbol-object))

(defpackage #:isol.map
  (:use #:cl
        #:isol.objects
        #:isol.utilities)
  (:export #:find-list-size
           #:list->array
           #:check-passability
           #:render-map
           #:load-map-from-file))

(defpackage #:isol.player
  (:use #:cl
        #:isol.map
        #:isol.objects
        #:isol.utilities))

(defpackage #:isol.graphics
  (:use #:cl
        #:isol.player
        #:isol.map
        #:isol.objects
        #:isol.utilities)
  (:export #:initialize-screen
           #:deinitialize-screen
           #:redraw-screen
           #:print-rendered-map
           #:print-map))

(defpackage #:isol.game
  (:use #:cl
        #:isol.player
        #:isol.map
        #:isol.objects
        #:isol.utilities))

(defpackage #:isol
  (:use #:cl
        #:isol.game
        #:isol.utilities))
