;;; jw-lang-ocaml --- OCaml

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package tuareg
  :ensure
  :mode ("\\.ml\\'" . tuareg-mode)
  :init
  (defun jw--tuareg-mode-hook ()
    "Hook to run on entering tuareg mode."
    (when (functionp 'prettify-symbols-mode)
      (prettify-symbols-mode)))
  :hook (tuareg-mode . jw--tuareg-mode-hook))

(use-package projectile :ensure)
(use-package spinner :ensure :pin elpa)
(use-package lsp-mode
  :ensure
  :after spinner
  :hook
  (tuareg-mode . lsp)
  (caml-mode . lsp))

(use-package lsp-ui
  :ensure
  :commands lsp-ui-mode
  :custom-face
  (lsp-ui-sideline-current-symbol
   ((t (:height 0.99
        :weight ultra-bold
        :box (:line-width -1 :color "dim gray" :style nil)
        :foreground "dim gray"))))

  )
(use-package company-lsp :ensure :commands company-lsp)


(provide 'jw-lang-ocaml)
;;; jw-lang-ocaml ends here
