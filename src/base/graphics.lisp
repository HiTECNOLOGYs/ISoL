(in-package :isol)

(defparameter *screen-initialized?* nil)
(define-constant +drawing-offset+ (cons 1 1)
  :test #'equal)

(defun initialize-screen (&rest arguments)
  "Initializes ncurses and sets some parameters."
  (unless *screen-initialized?*
    (initscr)
    (setf *screen-initialized?* t))
  (dolist (argument arguments)
    (case argument
      (:echo (echo))
      (:noecho (noecho))
      (:raw (raw))
      (:noraw (noraw))
      (:cbreak (cbreak))
      (:nocbreak (nocbreak))
      (:cursor (curs-set 1))
      (:nocursor (curs-set 0))))
  *screen-initialized?*)

(defun deinitialize-screen ()
  "Deinitializes ncurses."
  (when *screen-initialized?*
    (endwin)
    (setf *screen-initialized?* nil))
  *screen-initialized?*)

(defun clear-screen ()
  "Completely clears screen."
  (erase)
  (clear-all-windows)
  (values))

(defun redraw-screen ()
  "Refreshes screen."
  (refresh)
  (redraw-all-windows)
  (values))

(defun wprintw-newline (window-id string)
  "Prints text to ncurses with new line at the end."
  (let ((window-ref (or (window-ref (get-window-by-id window-id)) *stdscr*)))
    (wprintw window-ref string)
    (destructuring-bind (column . row) (get-window-cursor-position window-ref)
      (declare (ignore column))
      (wmove window-ref (1+ row) (cdr +drawing-offset+)))))

(defun wprintw-newline-limited (window-id length string &optional replacement)
  (if (> (length string) length)
    (wprintw-newline window-id
                     (concatenate 'string
                                  (subseq string 0
                                          (- length 2 (length replacement)))
                                  replacement))
    (wprintw-newline window-id string)))

(defun draw-char-at (window-id char x y)
  "Sets some position in `window' to `char'."
  (mvwaddch (or (window-ref (get-window-by-id window-id)) *stdscr*)
            (+ (cdr +drawing-offset+) y)
            (+ (car +drawing-offset+) x)
            (char-int char)))

(defun print-rendered-map (rendered-map)
  "Prints already rendered map."
  (dolist (line rendered-map)
    (wprintw-newline :game-window (list->string line))))

(defun print-map (map)
  "Renders map and prints it."
  (print-rendered-map (render-map map)))

(defun get-screen-size ()
  "Returns size of terminal screen."
  (let (rows columns)
    (getmaxyx *stdscr* rows columns)
    (cons columns rows)))

(defun get-screen-center ()
  (destructuring-bind (x . y) (get-screen-size)
    (cons (ash x -1)
          (ash y -1))))

(defmacro with-screen ((&body arguments) &body body)
  "Gurantee that wrapped code will be executed after successful initialization of screen and that screen will be properly deinitialized after `body' execution."
  `(unwind-protect
        (progn (initialize-screen ,@arguments)
               ,@body)
     (deinitialize-screen)))

(defun get-attribute-name-from-keyword (attribute)
  "Converts keyword to ncurses attribute."
  (if (not (keywordp attribute))
    attribute
    (case attribute
      (:normal a_normal)
      (:standout a_standout)
      (:underline a_underline)
      (:reverse a_reverse)
      (:blink a_blink)
      (:dim a_dim)
      (:bold a_bold)
      (:protect a_protect)
      (:invis a_invis)
      (:altcharset a_altcharset))))

(defmacro with-attributes ((&body attributes) &body body)
  "Wrapped code will be executed with given attributes and after `body' execution attributes will be disabled."
  `(unwind-protect
        (progn
          (attron ,(apply #'logior
                          (iter
                            (for attribute in attributes)
                            (collecting (get-attribute-name-from-keyword attribute)))))
          ,@body)
     (attroff ,(apply #'logior
                      (iter
                        (for attribute in attributes)
                        (collecting (get-attribute-name-from-keyword attribute)))))))
