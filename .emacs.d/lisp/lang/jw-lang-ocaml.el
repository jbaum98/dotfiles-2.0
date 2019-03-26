;;; jw-lang-ocaml --- OCaml

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package projectile
  :commands
  projectile-mode
  projectile-project-root
  :config
  (setq projectile-completion-system 'ivy))

(use-package spinner
  :commands
  spinner-start
  spinner-stop)

(use-package lsp-mode
  :hook (tuareg-mode . lsp))

(use-package lsp-ui
  :hook (tuareg-mode . lsp-ui-mode)
  :custom-face
  (lsp-ui-sideline-current-symbol
   ((t (:height 0.99
        :weight ultra-bold
        :box (:line-width -1 :color "dim gray" :style nil)
        :foreground "dim gray")))))

(use-package lsp-ui-flycheck
  :commands lsp-ui-flycheck-enable
  :hook (tuareg-mode . (lambda () (lsp-ui-flycheck-enable 1))))

(use-package company-lsp :commands company-lsp)

(use-package flycheck
  :hook (tuareg-mode . flycheck-mode))

(use-package prog-mode
  :hook (tuareg-mode . prettify-symbols-mode))

(use-package tuareg
  :mode ("\\.ml\\'" . tuareg-mode))

(use-package ocp-indent
  :if (executable-find "ocp-indent")
  :hook (tuareg-mode . ocp-setup-indent))

(provide 'jw-lang-ocaml)
;;; jw-lang-ocaml ends here
