(in-package :isol)

(defun display-message-in-minibuffer (string)
  "Displays message in minibuffer until palyer moves."
  (let ((max-length (- (car *screen-size*) 2)))
    (clear-window-id :minibuffer)
    (reset-window-cursor-position-id :minibuffer)
    (wprintw (window-ref (get-window-by-id :minibuffer))
                        (subseq string 0 (min (1- max-length)
                                              (length string))))
    (redraw-window-id :minibuffer)
    (when (> (length string) max-length)
      (loop until (= (char-code #\Space) (wait-for-key)))
      (display-message-in-minibuffer (subseq string max-length)))))

(defun prompt-input (string choices)
  "Prompt user for a choice. `choices' are list of (CHAR RESULT)."
  (clear-window-id :minibuffer)
  (reset-window-cursor-position-id :minibuffer)
  (display-message-in-minibuffer string)
  (let ((char (code-char (wait-for-key))))
    (second (assoc char choices
                   :test #'equal))))

(defun draw-center-menu (window items)
  "Draws some lines at the center of some window and highlights line
  if it's selected. Format for items: (cons text selectd?)"
  (destructuring-bind (x . y) (window-size window)
    (iter
      (for item in items)
      (for print-y
           from (- (ash y -1) (ash (length items) -1))
           to   (+ (ash y -1) (ash (length items) -1)))
      (destructuring-bind (item-text . selected?) item
        (let ((print-x (- (ash x -1) (ash (length item-text) -1))))
          (if selected?
            (with-attributes (:underline)
              (mvwprintw (or (window-ref window) *stdscr*) print-y print-x item-text))
            (mvprintw print-y print-x item-text)))))))

(defun display-center-menu (window-id &rest items)
  "Displays menu where player can choise something by moving cursor up and down."
  (let ((selected-item 0))
    (clear-window-id window-id)
    (labels ((move-selection-up ()
               (when (< (1- (length items)) (incf selected-item))
                 (setf selected-item (1- (length items))))
               nil)
             (move-selection-down ()
               (when (> 0 (decf selected-item))
                 (setf selected-item 0))
               nil)
             (select-item ()
               selected-item))
      (with-temporary-key-bindings ((#\j #'move-selection-down)
                                    (#\k #'move-selection-up)
                                    (#\Return #'select-item))
        (iter
          (for key next (wait-for-key))
          (for result next (process-key key))
          (until result)
          (finally (return result))
          (after-each (draw-center-menu (get-window-by-id window-id)
                                        items)
                      (redraw-window-id window-id)))))))
