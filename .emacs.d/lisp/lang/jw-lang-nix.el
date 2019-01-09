;;; jw-lang-nix --- Nix

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(eval-when-compile
  (defvar company-backends))

(defun jw--nix-mode-hook ()
  "Hook to run in `nix-mode'."
  (set (make-local-variable 'company-backends)
       '(company-tabnine
         company-capf
         (company-abbrev company-dabbrev))))

(use-package nix-mode
  :ensure
  :pin melpa
  :mode "\\.nix\\'"
  :init
  (add-hook 'nix-mode-hook #'jw--nix-mode-hook)
  (add-hook 'nix-mode-hook #'company-mode)
  :custom
  (nix-indent-function #'nix-indent-line))

(provide 'jw-lang-nix)
;;; jw-lang-nix.el ends here
