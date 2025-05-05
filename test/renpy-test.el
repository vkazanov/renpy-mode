;;; renpy-test.el ---                                -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'renpy)

(ert-deftest test-renpy-imenu ()
  (with-temp-buffer-str
      "
# comment
label start:
    say \"This is a test\"
"
    (let ((index-alist (funcall imenu-create-index-function)))
      (should (equal (length index-alist) 1))
      (should (alist-get "start" index-alist nil nil #'string-equal)))))

(provide 'renpy-test)
;;; renpy-test.el ends here
