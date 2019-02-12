;;; jw-funcs-error.el --- Functions for handling errors

;;; Commentary:
;;

;;; Code:

(eval-when-compile
  (require 'dash))

(defmacro jw//error-delegate (flycheck-forms emacs-forms)
  "Decide which error API to delegate to.

Delegates to flycheck by evaluating FLYCHECK-FORMS if it is
enabled and the `next-error` buffer is not visible. Otherwise
delegates to regular Emacs `next-error` by evaluating
EMACS-FORMS."
  `(if (bound-and-true-p flycheck-mode)
      ,flycheck-forms
    ,emacs-forms))

(defun jw/next-error (&optional n reset)
  "Dispatch to flycheck or standard Emacs error. Ignore N and RESET."
  (interactive "P")
  (jw//error-delegate
   (call-interactively 'flycheck-next-error)
   (call-interactively 'next-error)))

(defun jw/previous-error (&optional n reset)
  "Dispatch to flycheck or standard Emacs error. Ignore N and RESET."
  (interactive "P")
  (jw//error-delegate
   (call-interactively 'flycheck-previous-error)
   (call-interactively 'previous-error)))

(defun jw/toggle-flycheck-error-list ()
  "Toggle flycheck's error list window.
If the error list is visible, hide it.  Otherwise, show it."
  (interactive)
  (-if-let (window (flycheck-get-error-list-window))
      (quit-window nil window)
    (flycheck-list-errors)))

(defvar flycheck-error-list-buffer)
(autoload 'flycheck-get-error-list-window "flycheck")
(autoload 'flycheck-list-errors "flycheck")

(defun jw/goto-flycheck-error-list ()
  "Open and go to the error list buffer."
  (interactive)
  (unless (get-buffer-window (get-buffer flycheck-error-list-buffer))
    (flycheck-list-errors)
    (switch-to-buffer-other-window flycheck-error-list-buffer)))

(provide 'jw-funcs-error)
;;; jw-funcs-error.el ends here
