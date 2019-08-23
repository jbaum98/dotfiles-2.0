
(use-package lsp-mode
  :hook (lsp-mode . lsp--flymake-setup)
  :commands lsp
  :custom
  (lsp-auto-configure nil)
  (lsp-auto-execute-action t)
  (lsp-auto-guess-root t)
  (lsp-auto-require-clients nil)
  (lsp-eldoc-render-all t)
  (lsp-enable-snippet nil))

(use-package company-lsp :commands company-lsp)
(use-package helm-lsp :commands helm-lsp-workspace-symbol)

(use-package projectile
  :commands projectile-project-p projectile-project-root)

(provide 'jw-core-lsp)
