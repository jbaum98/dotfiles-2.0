;;; jw-lang-nix --- Nix

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(defun jw--nix-mode-hook ()
  "Hook to run in `nix-mode'."
  (set (make-local-variable 'company-backends)
       '(company-tabnine
         company-capf
         (company-abbrev company-dabbrev))))

(use-package nix-mode
  :mode "\\.nix\\'"
  :hook
  (nix-mode . jw--nix-mode-hook)
  (nix-mode . company-mode)
  :custom
  (nix-indent-function #'nix-indent-line))

(provide 'jw-lang-nix)
;;; jw-lang-nix.el ends here
