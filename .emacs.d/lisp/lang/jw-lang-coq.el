;;; jw-lang-coq --- Coq

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package pg-init
  :mode ("\\.v\\'" . coq-mode)
  ;; Fix keybinding issues
  :hook (coq-mode . evil-normalize-keymaps)
  :custom
  (proof-splash-enable nil)
  (proof-three-window-mode-policy 'hybrid))

(defun jw--coq-company-backends ()
  "Hook to run on entering coq mode."
  (set (make-local-variable 'company-backends)
       '(company-coq-math-symbols-backend
         company-coq-choices-backend
         (company-coq-master-backend
          company-tabnine)
         (company-abbrev company-dabbrev))))

(use-package company-coq
  :hook
  (coq-mode . company-coq-mode)
  (company-coq-mode . jw--coq-company-backends)
  :custom-face
  (proof-eager-annotation-face ((t (:background "medium blue"))))
  (proof-error-face ((t (:background "dark red"))))
  (proof-warning-face ((t (:background "indianred3"))))
  :custom
  (coq-compile-before-require t)
  (coq-compile-auto-save 'ask-coq)
  (coq-compile-parallel-in-background t)
  (coq-max-background-compilation-jobs 'all-cpus))

(jw-mode-def proof-mode-map
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

(provide 'jw-lang-coq)
;;; jw-lang-coq.el ends here
