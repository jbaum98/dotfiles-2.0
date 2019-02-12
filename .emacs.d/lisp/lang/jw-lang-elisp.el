;;; jw-lang-elisp --- Emacs Lisp

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package)
  (use-package jw-core-lib :load-path "lisp/core"))

(use-package elisp-mode
  :hook
  (emacs-lisp-mode . company-mode)
  (emacs-lisp-mode . flycheck-mode)
  :init
  (add-hook 'emacs-lisp-mode-hook
   (jw/set-var-hook 'company-transformers '(jw--company-sort-tabnine-first)))
  (add-hook 'emacs-lisp-mode-hook
            (jw/set-var-hook 'company-backends
                             '((company-capf
                                :with
                                company-tabnine)
                               (company-abbrev company-dabbrev)))))

(provide 'jw-lang-elisp)
;;; jw-lang-elisp.el ends here
