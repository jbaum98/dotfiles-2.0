;;; jw-lang-elisp --- Emacs Lisp

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package company
  :defer)

(defun jw--emacs-lisp-mode-hook ()
  "Hook to run on `emacs-lisp-mode'."
  (set (make-local-variable 'company-transformers)
       '(jw--company-sort-tabnine-first))
  (set (make-local-variable 'company-backends)
       '((company-capf    ; emacs lisp specific
          :with
          company-tabnine ; super smart
         )
         (company-abbrev company-dabbrev))))

(use-package elisp-mode
  :init
  (add-hook 'emacs-lisp-mode-hook #'jw--emacs-lisp-mode-hook)
  (add-hook 'emacs-lisp-mode-hook #'company-mode))

(provide 'jw-lang-elisp)
;;; jw-lang-elisp.el ends here
