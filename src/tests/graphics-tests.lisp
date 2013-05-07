(in-package :isol)
(in-suite graphics-tests)

(test test-attrubutes
  (is (= cl-ncurses:a_normal (get-attribute-name-from-keyword :normal)))
  (is (= cl-ncurses:a_blink (get-attribute-name-from-keyword :blink)))
  (is (= cl-ncurses:a_invis (get-attribute-name-from-keyword :invis))))
