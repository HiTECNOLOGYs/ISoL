(in-package #:isol)

(defparameter *screen-initialized?* nil)

(defun initialize-screen (&rest arguments)
  "Initializes ncurses and sets some parameters."
  (unless *screen-initialized?*
    (cl-ncurses:initscr)
    (setf *screen-initialized?* t))
  (dolist (argument arguments)
    (case argument
      (:echo (cl-ncurses:echo))
      (:noecho (cl-ncurses:noecho))
      (:raw (cl-ncurses:raw))
      (:noraw (cl-ncurses:noraw))
      (:cbreak (cl-ncurses:cbreak))
      (:nocbreak (cl-ncurses:nocbreak))
      (:cursor (cl-ncurses:curs-set 1))
      (:nocursor (cl-ncurses:curs-set 0))))
  *screen-initialized?*)

(defun deinitialize-screen ()
  "Deinitializes ncurses."
  (when *screen-initialized?*
    (cl-ncurses:endwin)
    (setf *screen-initialized?* nil))
  *screen-initialized?*)

(defun clear-screen ()
  "Completely clears screen."
  (cl-ncurses:clear))

(defun redraw-screen ()
  "Refreshes screen."
  (cl-ncurses:refresh))

(defun printw-newline (string)
  "Prints text to ncurses with new line at the end."
  (cl-ncurses:printw (concatenate 'string
                                  string
                                  (coerce (list #\Newline) 'string))))

(defun print-rendered-map (rendered-map)
  "Prints already rendered map."
  (mapcar (compose #'printw-newline #'list->string) rendered-map))

(defun print-map (map)
  "Renders map and prints it."
  (print-rendered-map (render-map map)))

(defmethod print-object ((player Player) (stream (eql :game-window)))
  (destructuring-bind (player-x player-y) (location player)
    (cl-ncurses:mvaddch player-y
                        player-x
                        (char-int (display-character player)))))


(defmacro with-screen ((&body arguments) &body body)
  `(unwind-protect
        (progn (initialize-screen ,@arguments)
               ,@body)
     (deinitialize-screen)))

(defun get-attribute-name-from-keyword (attribute)
  (if (not (keywordp attribute))
    attribute
    (case attribute
      (:normal cl-ncurses:a_normal)
      (:standout cl-ncurses:a_standout)
      (:underline cl-ncurses:a_underline)
      (:reverse cl-ncurses:a_reverse)
      (:blink cl-ncurses:a_blink)
      (:dim cl-ncurses:a_dim)
      (:bold cl-ncurses:a_bold)
      (:protect cl-ncurses:a_protect)
      (:invis cl-ncurses:a_invis)
      (:altcharset cl-ncurses:a_altcharset))))

(defmacro with-attributes ((&body attributes) &body body)
  `(unwind-protect
        (progn (cl-ncurses:attron ,(apply #'logior
                                          (loop for attribute in attributes
                                                collecting (get-attribute-name-from-keyword attribute))))
               ,@body)
     (progn (cl-ncurses:attroff ,(apply #'logior
                                        (loop for attribute in attributes
                                              collecting (get-attribute-name-from-keyword attribute)))))))
