(in-package :isol)

(defun display-message-in-minibuffer (string max-length)
  "Displays message in minibuffer and waits until player presses <Space>. If message is too big for minibuffer, it will display it by pieces."
  (clear-window :minibuffer)
  (cl-ncurses:mvwprintw (get-window-by-id :minibuffer)
                        (cdr +drawing-offset+)
                        (car +drawing-offset+)
                        (concatenate 'string
                                     (subseq string 0 (min (1- max-length)
                                                           (length string)))
                                     " <Press Space>"))
  (redraw-screen)
  (loop until (= (char-code #\Space) (wait-for-key)))
  (when (> (length string) max-length)
    (display-message-in-minibuffer (subseq string max-length) max-length))))

(defun prompt-input (string choices)
  (clear-window :minibuffer)
  (cl-ncurses:mvwprintw (get-window-by-id :minibuffer)
                        (cdr +drawing-offset+)
                        (car +drawing-offset+)
                        (format nil "~A [~{~A~}]" string (mapcar #'first choices)))
  (redraw-screen)
  (let ((char (code-char (wait-for-key))))
    (second (assoc char choices
                   :test #'equal))))
