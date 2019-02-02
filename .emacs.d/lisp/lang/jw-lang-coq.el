;;; jw-lang-coq --- Coq

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package pg-init
  :ensure proof-general
  :mode ("\\.v\\'" . coq-mode)
  :custom
  (proof-splash-enable nil)
  (proof-three-window-mode-policy 'hybrid))

(defun jw--coq-mode-hook ()
  "Hook to run on entering coq mode."
  (general-define-key
   :states '(normal insert visual motion emacs)
   :prefix jw-leader-key
   :non-normal-prefix jw-emacs-leader-key
   :keymaps 'local
   "m" 'jw-coq-leader-cmd)
  (flycheck-mode -1)
  (set (make-local-variable 'company-backends)
       '(company-coq-math-symbols-backend
         company-coq-choices-backend
         (company-coq-master-backend
          company-tabnine)
         (company-abbrev company-dabbrev))))

(use-package company-coq
  :ensure
  :commands company-coq-mode
  :hook (coq-mode . company-coq-mode)
  :init
  (add-hook 'coq-mode-hook #'jw--coq-mode-hook)
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
  :custom-face
  (proof-eager-annotation-face ((t (:background "medium blue"))))
  (proof-error-face ((t (:background "dark red"))))
  (proof-warning-face ((t (:background "indianred3"))))
  :custom
  (coq-compile-before-require t)
  (coq-compile-auto-save 'ask-coq)
  (coq-compile-parallel-in-background t)
  (coq-max-background-compilation-jobs 'all-cpus))

(provide 'jw-lang-coq)
;;; jw-lang-coq.el ends here
