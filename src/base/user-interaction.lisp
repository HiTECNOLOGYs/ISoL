(in-package :isol)

(defun display-message (string max-length)
  (if (> (length string) max-length)
    (progn (wprintw-newline-limited :minibuffer max-length string "...<Space>")
           (wait-for-key)
           (display-message (subseq string max-length) max-length))
    (wprintw-newline :minibuffer string)))
