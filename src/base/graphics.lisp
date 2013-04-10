(in-package #:isol)

(defun initialize-screen ()
  "Initializes ncurses and sets some parameters."
  (cl-ncurses:initscr))

(defun deinitialize-screen ()
  "Deinitializes ncurses."
  (cl-ncurses:endwin))

(defun clear-screen ()
  (cl-ncurses:clear))

(defun redraw-screen ()
  "Refreshes screen."
  (cl-ncurses:refresh))

(defun print-rendered-map (rendered-map)
  "Prints already rendered map."
  (mapcar (compose #'cl-ncurses:printw #'list->string) rendered-map))

(defun print-map (map)
  "Renders map and prints it."
  (print-rendered-map (render-map map)))

(defun print-player (player)
  "Prints player on his(her?) position on the map for now.
Will print some other info in the future, I think."
  (cl-ncurses:mvaddch (player-y player)
                      (player-x player)
                      (char-int (player-character player))))
