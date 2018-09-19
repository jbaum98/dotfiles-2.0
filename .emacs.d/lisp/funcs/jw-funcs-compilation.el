;;; jw-funcs-compilation.el --- Functions for compilation

;;; Commentary:

;;; Code:

(defvar compilation-last-buffer)

(defun jw/close-compilation-window ()
  "Close the window containing the '*compilation*' buffer."
  (interactive)
  (when compilation-last-buffer
    (delete-windows-on compilation-last-buffer)))

(provide 'jw-funcs-compilation)
;;; jw-funcs-compilation.el ends here
