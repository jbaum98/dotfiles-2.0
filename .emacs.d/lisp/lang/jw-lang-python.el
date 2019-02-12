;;; jw-lang-python --- Python

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package)
  (use-package jw-core-lib :load-path "lisp/core"))

(use-package python
  :hook
  (python-mode . company-mode)
  (python-mode . flycheck-mode)
  :init
  (add-hook 'python-mode-hook
   (jw/set-var-hook 'company-backends
                    '((company-tabnine)
                      (company-abbrev company-dabbrev)))))

;; (use-package blacken
;;   :ensure
;;   :pin melpa
;;   :hook (python-mode . blacken-mode))

;; (use-package isortify
;;   :ensure
;;   :pin melpa
;;   :hook (python-mode . isortify-mode))

(provide 'jw-lang-python)
;;; jw-lang-python.el ends here
