;;; renpy-test.el ---                                -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'renpy)

(ert-deftest test-renpy-imenu-basic ()
  (with-temp-buffer-str
      "
# comment
label start:
    say \"This is a test\"
"
    (let ((index-alist (funcall imenu-create-index-function)))
      (should (equal (length index-alist) 1))
      (should (alist-get "start" index-alist nil nil #'string-equal)))))

(ert-deftest test-renpy-imenu-multiple ()
  (with-temp-buffer-str
      "
# comment
label first:
    say \"first label\"

label second_label:
    say \"second label\"

label .local_label:
    say \"local label\"

label label_with_params(a):
    say \"paremetrized label\"

"
    (let ((index-alist (funcall imenu-create-index-function)))
      (should (equal (length index-alist) 4))
      (should (alist-get "first" index-alist nil nil #'string-equal))
      (should (alist-get "second_label" index-alist nil nil #'string-equal))
      (should (alist-get ".local_label" index-alist nil nil #'string-equal))
      (should (alist-get "label_with_params" index-alist nil nil #'string-equal)))))

(provide 'renpy-test)
;;; renpy-test.el ends here
