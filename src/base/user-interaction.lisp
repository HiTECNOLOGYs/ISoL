(in-package :isol)

(defun display-message-in-minibuffer (string max-length)
  "Displays message in minibuffer and waits until player presses <Space>. If message is too big for minibuffer, it will display it by pieces."
  (clear-window :minibuffer)
  (push-drawing-task #'cl-ncurses:mvwprintw
                     (get-window-by-id :minibuffer)
                     (cdr +drawing-offset+)
                     (car +drawing-offset+)
                     (subseq string 0 (min (1- max-length)
                                           (length string))))
  (when (> (length string) max-length)
    (loop until (= (char-code #\Space) (wait-for-key)))
    (display-message-in-minibuffer (subseq string max-length) max-length)))

(defun prompt-input (string choices)
  "DEPRECATED"
  (clear-window :minibuffer)
  (cl-ncurses:mvwprintw (get-window-by-id :minibuffer)
                        (cdr +drawing-offset+)
                        (car +drawing-offset+)
                        (format nil "~A [~{~A~}]" string (mapcar #'first choices)))
  (let ((char (code-char (wait-for-key))))
    (second (assoc char choices
                   :test #'equal))))
