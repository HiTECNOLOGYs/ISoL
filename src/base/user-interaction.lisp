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
