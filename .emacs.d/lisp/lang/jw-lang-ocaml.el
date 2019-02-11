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
  :commands lsp)

(use-package lsp-ui
  :commands lsp-ui-mode
  :custom-face
  (lsp-ui-sideline-current-symbol
   ((t (:height 0.99
        :weight ultra-bold
        :box (:line-width -1 :color "dim gray" :style nil)
        :foreground "dim gray")))))

(use-package company-lsp :commands company-lsp)

(use-package tuareg
  :mode ("\\.ml\\'" . tuareg-mode)
  :hook (tuareg-mode . prettify-symbols-mode)
  :hook (tuareg-mode . lsp)
  :hook (tuareg-mode . lsp-ui-mode))


(provide 'jw-lang-ocaml)
;;; jw-lang-ocaml ends here
