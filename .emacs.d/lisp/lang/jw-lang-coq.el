;;; jw-lang-coq --- Coq

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package proof-site
  :load-path "~/.nix-profile/share/emacs/site-lisp/ProofGeneral/generic"
  :mode ("\\.v\\'" . coq-mode))

(use-package company-coq
  :ensure
  :commands company-coq-mode
  :hook (coq-mode . company-coq-mode)
  :init
  (add-hook
   'coq-mode-hook
   (lambda ()
     (general-define-key
      :states '(normal insert visual motion emacs)
      :prefix jw-leader-key
      :non-normal-prefix jw-emacs-leader-key
      :keymaps 'local
      "m" 'jw-coq-leader-cmd)))
  :general
  (general-define-key
   :states '(normal insert visual motion emacs)
   :prefix jw-mode-key
   :non-normal-prefix jw-emacs-mode-key
   :prefix-map 'jw-coq-leader-map
   :prefix-command 'jw-coq-leader-cmd
   :keymaps 'proof-mode-map
    "n" 'proof-assert-next-command-interactive
    "]" 'proof-assert-next-command-interactive
    "u" 'proof-undo-last-successful-command
    "[" 'proof-undo-last-successful-command
    "h" 'company-coq-doc
    "ll" 'proof-layout-windows
    "lp" 'proof-prf
    "x" 'proof-shell-exit
    "s" 'proof-find-theorems
    "?" 'coq-Check
    "p" 'coq-Print
    ";" 'pg-insert-last-output-as-comment
    "o" 'company-coq-occur
    "." 'proof-goto-point)
  :config
  (custom-set-faces
   '(proof-eager-annotation-face ((t (:background "medium blue"))))
   '(proof-error-face ((t (:background "dark red"))))
   '(proof-warning-face ((t (:background "indianred3")))))
  :custom
  (proof-three-window-mode-policy 'hybrid))

(provide 'jw-lang-coq)
;;; jw-lang-coq.el ends here
