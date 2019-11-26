;;; jw-lang-go --- Go

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package go-mode
  :init
  (add-hook 'go-mode-hook
            (lambda () (add-hook 'before-save-hook 'gofmt-before-save))))

(provide 'jw-lang-go)
;;; jw-lang-go ends here
