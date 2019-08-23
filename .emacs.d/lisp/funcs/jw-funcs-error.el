;;; jw-funcs-error.el --- Functions for handling errors

;;; Commentary:
;;

;;; Code:

(eval-when-compile
  (require 'dash))

(defmacro jw//error-delegate (flycheck-forms flymake-forms emacs-forms)
  "Decide which error API to delegate to.

Delegates to flycheck by evaluating FLYCHECK-FORMS if it is
enabled and the `next-error` buffer is not visible. Otherwise
delegates to regular Emacs `next-error` by evaluating
EMACS-FORMS."
  `(cond
    ((bound-and-true-p flycheck-mode) ,flycheck-forms)
    ((bound-and-true-p flymake-mode) ,flymake-forms)
    (t ,emacs-forms)))

(defun jw/next-error (&optional n reset)
  "Dispatch to flycheck or standard Emacs error. Ignore N and RESET."
  (interactive "P")
  (jw//error-delegate
   (call-interactively 'flycheck-next-error)
   (call-interactively 'flymake-goto-next-error)
   (call-interactively 'next-error)))

(defun jw/previous-error (&optional n reset)
  "Dispatch to flycheck or standard Emacs error. Ignore N and RESET."
  (interactive "P")
  (jw//error-delegate
   (call-interactively 'flycheck-previous-error)
   (call-interactively 'flymake-goto-prev-error)
   (call-interactively 'previous-error)))

(defun jw/toggle-error-list ()
  "Toggle error list window.
If the error list is visible, hide it.  Otherwise, show it."
  (interactive)
  (jw//error-delegate
   (call-interactively 'flycheck-list-errors)
   (call-interactively 'flymake-show-diagnostics-buffer)
   (progn)))

(provide 'jw-funcs-error)
;;; jw-funcs-error.el ends here
