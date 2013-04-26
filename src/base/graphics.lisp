(in-package #:isol)

(defparameter *screen-initialized?* nil)

(defun initialize-screen (&rest arguments)
  "Initializes ncurses and sets some parameters."
  (unless *screen-initialized?*
    (cl-ncurses:initscr)
    (setf *screen-initialized?* t))
  (dolist (argument arguments)
    (case argument
      (:noecho (cl-ncurses:noecho))
      (:raw (cl-ncurses:raw))
      (:nocbreak (cl-ncurses:nocbreak))
      (:nocursor (cl-ncurses:curs-set 0))
      (:noraw (cl-ncurses:noraw))
      (:echo (cl-ncurses:echo))))
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
