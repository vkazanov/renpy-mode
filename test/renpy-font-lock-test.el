;;; renpy-font-lock-test.el ---                     -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'renpy)

;; NOTE: ert-font-lock was only introduced in Emacs 30.
(when (>= emacs-major-version 30)
  (require 'ert-font-lock)
  (ert-font-lock-deftest test-renpy-font-lock-basic renpy-mode
    "
label start:
# <- font-lock-keyword-face
#^^^^font-lock-keyword-face
#     ^^^^^ font-lock-function-name-face
    say \"Hello, World!\"
"))

;;; renpy-font-lock-test.el ends here
