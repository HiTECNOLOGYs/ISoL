(in-package #:isol)

(defun initialize-screen ()
  "Initializes ncurses and sets some parameters."
  (cl-ncurses:initscr))

(defun deinitialize-screen ()
  "Deinitializes ncurses."
  (cl-ncurses:endwin))

(defun redraw-screen ()
  "Refreshes screen."
  (cl-ncurses:refresh))

(defun print-rendered-map (rendered-map)
  "Prints already rendered map."
  (mapcar (compose #'cl-ncurses:printw #'list->string) rendered-map))

(defun print-map (map)
  "Renders map and prints it."
  (print-rendered-map (render-map map)))
