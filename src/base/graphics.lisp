(in-package :isol)

(defparameter *screen-initialized?* nil)
(define-constant +drawing-offset+ (cons 1 1))

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
  (cl-ncurses:erase)
  (clear-all-windows)
  (values))

(defun redraw-screen ()
  "Refreshes screen."
  (cl-ncurses:refresh)
  (redraw-all-windows)
  (values))

(defun wprintw-newline (window-id string)
  "Prints text to ncurses with new line at the end."
  (let ((window-ref (or (window-ref (get-window-by-id window-id)) cl-ncurses:*stdscr*)))
    (cl-ncurses:wprintw window-ref
                        string)
    (destructuring-bind (column . row) (get-window-cursor-position window-ref)
      (declare (ignore column))
      (cl-ncurses:wmove window-ref (1+ row) (cdr +drawing-offset+)))))

(defun wprintw-newline-limited (window-id length string &optional replacement)
  (if (> (length string) length)
    (wprintw-newline window-id (concatenate 'string
                                            (subseq string 0
                                                    (- length 2 (length replacement)))
                                            replacement))
    (wprintw-newline window-id string)))

(defun draw-char-at (window-id char x y)
  "Sets some position in `window' to `char'."
  (cl-ncurses:mvwaddch (or (window-ref (get-window-by-id window-id)) cl-ncurses:*stdscr*)
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
    (cl-ncurses:getmaxyx cl-ncurses:*stdscr* rows columns)
    (cons columns rows)))

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
  "Wrapped code will be executed with given attributes and after `body' execution attributes will be disabled."
  `(unwind-protect
        (progn (cl-ncurses:attron ,(apply #'logior
                                          (loop for attribute in attributes
                                                collecting (get-attribute-name-from-keyword attribute))))
               ,@body)
     (cl-ncurses:attroff ,(apply #'logior
                                 (loop for attribute in attributes
                                    collecting (get-attribute-name-from-keyword attribute))))))
