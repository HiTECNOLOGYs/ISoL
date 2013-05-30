(in-package :isol)

(defparameter *windows* nil)

(defun get-window-by-id (window-id)
  (assoc window-id *windows*))

(defun (setf get-window-by-id) (new-value window-id)
  (setf (assoc window-id *windows*)
        new-value))

(defun make-window (window-id window-x window-y window-columns window-rows &key have-box? initial-scene)
  (list window-id
        (cl-ncurses:newwin window-rows
                           window-columns
                           window-y
                           window-x)
        have-box?
        (when initial-scene
          (list initial-scene))))

(defun window-id (window)
  (first window))

(defun window-ref (window)
  (second window))

(defun window-have-box-p (window)
  (third window))

(defun window-scenes (window)
  (fourth window))


(defun (setf window-id) (new-value window)
  (setf (first window)
        new-value))

(defun (setf window-ref) (new-value window)
  (setf (second window)
        new-value))

(defun (setf window-have-box-p) (new-value window)
  (setf (third window)
        new-value))

(defun (setf window-scenes) (new-value window)
  (setf (fourth window)
        new-value))


(defun push-window-scene (window-id scene)
  "Pushes scene from window."
  (push scene
        (window-scenes (get-window-by-id window-id)))
  scene)

(defun pop-window-scene (window-id)
  "Pops scene from window."
  (pop (window-scenes (get-window-by-id window-id))))


(defun render-window-scene (window-id &rest arguments)
  "Calls window rendering function with some arguments."
  (apply (first (window-scenes (get-window-by-id window-id)))
         arguments))


(defun make-new-window (window-id window-x window-y window-columns window-rows &key have-box? initial-scene)
  "Adds new window."
  (let ((window (make-window window-id
                             window-x window-y
                             window-columns window-rows
                             :have-box? have-box?
                             :initial-scene initial-scene)))
    (pushnew window
             *windows*
             :key #'window-id)
    window))

(defun remove-window (window-id)
  "Removes window with given ID."
  (when-let (window (get-window-by-id window-id))
    (cl-ncurses:delwin (window-ref window))
    (setf *windows*
          (remove window-id
                  *windows*
                  :key #'window-id)))
  window)

(defun remove-all-windows ()
  "Clears windows list and removes all windows references from ncurses."
  (dolist (window *windows*)
    (cl-ncurses:delwin (window-ref window)))
  (setf *windows* nil)
  (values))


(defun get-window-cursor-position (window-ref)
  "Returns cons where CAR is X coordinate of cursor position and CDR are Y."
  (let (x y)
    (cl-ncurses:getyx (or window-ref cl-ncurses:*stdscr*)
                      y
                      x)
    (cons x y)))


(defun draw-window-box (window-ref)
  "Draws box around given window."
  (cl-ncurses:box (or window-ref cl-ncurses:*stdscr*)
                  (char-code #\|)
                  (char-code #\-))
  (values))


(defun reset-window-cursor-position (window-ref)
  "Sets cursor position in given window to position from +drawing-offset+. Usually it's (1; 1)."
  (cl-ncurses:wmove window-ref
                    (cdr +drawing-offset+)
                    (car +drawing-offset+))
  (get-window-cursor-position window-ref))

(defun redraw-window (window-ref &optional draw-box?)
  "Redraws given window."
  (let ((window-ref (or window-ref cl-ncurses:*stdscr*)))
    (reset-window-cursor-position window-ref)
    (when draw-box?
      (draw-window-box window-ref))
    (cl-ncurses:wrefresh window-ref))
  (values))

(defun clear-window (window-ref)
  "Replaces all window characters with spaces, basically clearing it completely."
  (let ((window-ref (or window-ref (cl-ncurses:*stdscr*))))
    (cl-ncurses:werase window-ref))
  (values))

(defun clear-all-windows ()
  (mapc (compose #'clear-window #'window-ref)
        *windows*)
  (values))

(defun reset-all-windows-cursor-position ()
  (mapc (compose #'reset-window-cursor-position #'window-ref)
        *windows*)
  (values))

(defun redraw-all-windows ()
  (dolist (window *windows*)
    (redraw-window (window-ref window)
                   (window-have-box-p window)))
  (values))


(defun clear-window-id (window-id)
  (clear-window (window-ref (get-window-by-id window-id))))

(defun redraw-window-id (window-id)
  (redraw-window (window-ref (get-window-by-id window-id))))

(defun reset-window-cursor-position-id (window-id)
  (reset-window-cursor-position (window-ref (get-window-by-id window-id))))
