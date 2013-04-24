(in-package #:isol)

(defun initialize-screen ()
  "Initializes ncurses and sets some parameters."
  (cl-ncurses:initscr))

(defun deinitialize-screen ()
  "Deinitializes ncurses."
  (cl-ncurses:endwin))

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

(defmethod print-object ((player Player) (stream (eql '*game-window*)))
  (destructuring-bind (player-x player-y) (location player)
    (cl-ncurses:mvaddch player-y
                        player-x
                        (char-int (display-character player)))))
