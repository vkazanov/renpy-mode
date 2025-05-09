;;; test-helper.el --- Helpers                       -*- lexical-binding: t; -*-

;; NOTE: test/test-helper.el is always loaded by ert-runner prior to running
;; unit tests. If manual test run is necessary then this file has to be loaded
;; manually.

;;; Code:

(defmacro with-temp-buffer-str (str &rest body)
  "Eval BODY within a buffer with containing STR."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (insert ,str)
     (renpy-mode)
     (goto-char (point-min))
     ,@body))

;;; test-helper.el ends here
