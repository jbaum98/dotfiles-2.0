;;; jw-lang-nix --- Nix

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package)
  (use-package jw-core-lib :load-path "lisp/core"))

(use-package nix-mode
  :mode "\\.nix\\'"
  ;; TODO: use :functions
  :commands nix-indent-line
  :hook
  (nix-mode . company-mode)
  :init
  (add-hook 'nix-mode-hook
            (jw/set-var-hook 'company-backends
                             '(company-tabnine
                               company-capf
                               (company-abbrev company-dabbrev))))
  :custom
  (nix-indent-function #'nix-indent-line))

(provide 'jw-lang-nix)
;;; jw-lang-nix.el ends here
