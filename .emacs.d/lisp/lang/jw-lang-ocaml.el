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
      (prettify-symbols-mode))
    ;; Disable Merlin's own error checking
    (setq merlin-error-after-save nil)
    ;; Enable Flycheck checker
    (flycheck-ocaml-setup))
  :hook (tuareg-mode . jw--tuareg-mode-hook))

(use-package merlin
  :ensure
  :hook (tuareg-mode . merlin-mode)
  :custom
  (merlin-command "ocamlmerlin")
  )

(use-package flycheck-ocaml
  :ensure)

(provide 'jw-lang-ocaml)
;;; jw-lang-ocaml ends here
