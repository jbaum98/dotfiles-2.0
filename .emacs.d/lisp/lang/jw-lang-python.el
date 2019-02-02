;;; jw-lang-python --- Python

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package company
  :defer)

(defun jw--python-mode-hook ()
  "Hook to run on `python-mode'."
  (set (make-local-variable 'company-backends)
       '((company-tabnine)
         (company-abbrev company-dabbrev)))
  (company-mode 1)
  (flycheck-mode 1))

(add-hook 'python-mode-hook #'jw--python-mode-hook)

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
