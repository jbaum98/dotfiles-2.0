;;; jw-lang-go --- Go

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))
(require 'jw-core-keybindings)

(use-package go-mode
  :ensure
  :mode "\\.go\\'"
  :hook (before-save . gofmt-before-save))

(autoload 'company-mode "company")
(defvar company-backends)
(use-package company-go
  :ensure
  :requires company
  :hook
  (go-mode . (lambda ()
               (set (make-local-variable 'company-backends) '(company-go))
               (company-mode))))

(provide 'jw-lang-go)
;;; jw-lang-go.el ends here
