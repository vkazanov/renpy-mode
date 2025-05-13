;;; renpy-flymake-test.el ---  -*- lexical-binding: t; -*-

;;; Commentary:

;; Flymake backend tests.

;;; Code:

(require 'ert)
(require 'flymake)
(require 'renpy)

(ert-deftest renpy--parse-lint-buffer-test ()
  (let ((output-buffer (generate-new-buffer "*renpy-lint-output*")))
    (unwind-protect
        (progn
          (with-current-buffer output-buffer
            (insert "game/script.rpy:26 The call is to nonexistent label 'chapter12'.\n")
            (insert "game/script.rpy:29 The call is to nonexistent label 'chapter14'.\n")
            (insert "File \"script.rpy\", line 27: expected statement.\n"))
          (let ((diags (renpy--parse-lint-buffer output-buffer)))
            (should (= (length diags) 3))
            (should (string-equal "The call is to nonexistent label 'chapter12'."
                                  (flymake-diagnostic-text (nth 2 diags))))
            (should (string-equal "The call is to nonexistent label 'chapter14'."
                                  (flymake-diagnostic-text (nth 1 diags))))
            (should (string-equal "expected statement."
                                  (flymake-diagnostic-text (nth 0 diags))))))
      (when (buffer-live-p output-buffer)
        (kill-buffer output-buffer)))))

(ert-deftest renpy--parse-lint-buffer-invalid-test ()
  (let ((output-buffer (generate-new-buffer "*renpy-lint-output*")))
    (unwind-protect
        (progn
          (with-current-buffer output-buffer
            ;; Expect 1 valid diagnostic case and ignore the second invalid format.
            (insert "File \"random/file.rpy\", line 100: Another kind of warning.\n")
            (insert "Invalid message format without a line number.\n"))
          (let ((diags (renpy--parse-lint-buffer output-buffer)))
            (should (= (length diags) 1))
            (should (string-equal "Another kind of warning."
                                  (flymake-diagnostic-text (nth 0 diags))))))
      (when (buffer-live-p output-buffer)
        (kill-buffer output-buffer)))))

;;; renpy-flymake-test.el ends here
