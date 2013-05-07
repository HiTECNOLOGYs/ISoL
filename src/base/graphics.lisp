(in-package :isol)

(defparameter *screen-initialized?* nil)
(defparameter *windows* nil)
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
  (mapc (compose #'cl-ncurses:werase #'second)
        *windows*))

(defun clear-window (window-id)
  "Clears window with given `window-id'."
  (cl-ncurses:werase (or (get-window-by-id window-id) cl-ncurses:*stdscr*)))

(defun reset-cursor-position (window-id)
  (cl-ncurses:wmove (or (get-window-by-id window-id) cl-ncurses:*stdscr*)
                    (car +drawing-offset+) (cdr +drawing-offset+)))

(defun reset-all-windows-cursor-positions ()
  (mapc (compose #'reset-cursor-position #'first) *windows*))

(defun redraw-screen ()
  "Refreshes screen."
  (dolist (window *windows*)
    (reset-cursor-position (first window))
    (when (third window)
      (draw-window-box (first window))))
  (cl-ncurses:refresh)
  (dolist (window *windows*)
    (cl-ncurses:wrefresh (second window))))

(defun redraw-window (window-id)
  "Redraws windows with given `window-id'."
  (let ((window-ref (get-window-by-id window-id)))
    (cl-ncurses:wmove window-ref
                      (car +drawing-offset+)
                      (cdr +drawing-offset+))
    (cl-ncurses:wrefresh window-ref)))

(defun wprintw-newline (window string)
  "Prints text to ncurses with new line at the end."
  (let ((window-ref (or (get-window-by-id window) cl-ncurses:*stdscr*)))
    (cl-ncurses:wprintw window-ref
                        string)
    (destructuring-bind (column . row) (get-cursor-position window)
      (declare (ignore column))
      (cl-ncurses:wmove window-ref (1+ row) (cdr +drawing-offset+)))))

(defun wprintw-newline-limited (window length string &optional replacement)
  (if (> (length string) length)
    (wprintw-newline window (concatenate 'string (subseq string 0
                                                         (- length 2 (length replacement)))
                                         replacement))
    (wprintw-newline window string)))

(defun draw-char-at (window char x y)
  "Sets some position in `window' to `char'."
  (cl-ncurses:mvwaddch (or (get-window-by-id window) cl-ncurses:*stdscr*)
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

(defun get-cursor-position (&optional window)
  "Returns current cursor position in some `window'."
  (let (row column)
    (cl-ncurses:getyx (or (get-window-by-id window) cl-ncurses:*stdscr*)
                      row
                      column)
    (cons column row)))

(defun create-new-window (id x-position y-position x-size y-size &key with-box?)
  "Creates new window with given `id', position and size."
  (pushnew (list id
                 (cl-ncurses:newwin y-size x-size y-position x-position)
                 with-box?)
           *windows*
           :key #'first))

(defun delete-window (id)
  (when-let (window (get-window-by-id id))
    (setf *windows* (remove id *windows*
                            :key #'first))
    (cl-ncurses:delwin window)))

(defun draw-window-box (id)
  "Draws box around the window with given `id'."
  (cl-ncurses:box (or (get-window-by-id id) cl-ncurses:*stdscr*)
                  (char-code #\|)
                  (char-code #\-)))

(defun get-window-by-id (id)
  "Returns window reference for given window `id'."
  (second (assoc id *windows*)))

(defun delete-windows ()
  "Deteles all existed windows."
  (setf *windows* nil))

(defmacro with-screen ((&body arguments) &body body)
  "Gurantee that wrapped code will be executed after successful initialization of screen and that screen will be properly deinitialized after `body' execution."
  `(unwind-protect
        (progn (initialize-screen ,@arguments)
               ,@body)
     (deinitialize-screen)))

(defun get-attribute-name-from-keyword (attribute)
  "Converts list keyword to ncurses attribute id."
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
     (progn (cl-ncurses:attroff ,(apply #'logior
                                        (loop for attribute in attributes
                                              collecting (get-attribute-name-from-keyword attribute)))))))
