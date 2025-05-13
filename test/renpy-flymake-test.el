;;; renpy-flymake-test.el --- Unit tests for the flymake backend -*- lexical-binding: t; -*-

(require 'ert)
(require 'flymake)

(ert-deftest renpy--parse-lint-buffer-test ()
  (let ((output-buffer (generate-new-buffer "*renpy-lint-output*")))
    (with-current-buffer output-buffer
      (insert "game/script.rpy:26 The call is to nonexistent label 'chapter12'.\n")
      (insert "game/script.rpy:29 The call is to nonexistent label 'chapter14'.\n")
      (insert "File \"script.rpy\", line 27: expected statement.\n"))
    (let ((diags (renpy--parse-lint-buffer output-buffer)))
      (should (= (length diags) 3))
      (should (string-equal "The call is to nonexistent label 'chapter12'." (flymake-diagnostic-text (nth 2 diags))))
      (should (string-equal "The call is to nonexistent label 'chapter14'." (flymake-diagnostic-text (nth 1 diags))))
      (should (string-equal "expected statement." (flymake-diagnostic-text (nth 0 diags)))))
    (kill-buffer output-buffer)))
