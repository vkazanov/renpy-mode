;;; test-helper.el --- Helpers                       -*- lexical-binding: t; -*-

(defmacro with-temp-buffer-str (str &rest body)
  "Eval BODY within a buffer with containing STR. "
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (insert ,str)
     (renpy-mode)
     (goto-char (point-min))
     ,@body))

;;; test-helper.el ends here
