;;; jw-core-autocomplete.el --- Config for yasnippet and company, and flycheck

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package company
  :ensure
  :defer 1
  :diminish company-mode
  :commands global-company-mode
  :bind
  (:map company-mode-map
   ("M-/" . company-complete))
  (:map company-active-map
   ("M-n" . company-select-next)
   ("M-p" . company-select-previous)
   ("C-j" . company-select-next)
   ("C-k" . company-select-previous)
   ("TAB" . company-complete-selection))
  :config
  (global-company-mode)
  (setq tab-always-indent 'complete))

(use-package flycheck
  :pin melpa-stable
  :ensure
  :defer 3
  :diminish flycheck-mode
  :custom
  (flycheck-emacs-lisp-load-path 'inherit)
  :commands
  flycheck-define-error-level
  global-flycheck-mode
  :config
  (define-fringe-bitmap 'flycheck-fringe-bitmap-ball
    (vector #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00011100
            #b00111110
            #b00111110
            #b00111110
            #b00011100
            #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00000000))
  (flycheck-define-error-level 'error
    :severity 2
    :overlay-category 'flycheck-error-overlay
    :fringe-bitmap 'flycheck-fringe-bitmap-ball
    :fringe-face 'flycheck-fringe-error)
  (flycheck-define-error-level 'warning
    :severity 1
    :overlay-category 'flycheck-warning-overlay
    :fringe-bitmap 'flycheck-fringe-bitmap-ball
    :fringe-face 'flycheck-fringe-warning)
  (flycheck-define-error-level 'info
    :severity 0
    :overlay-category 'flycheck-info-overlay
    :fringe-bitmap 'flycheck-fringe-bitmap-ball
    :fringe-face 'flycheck-fringe-info)

  (cl-loop for (key desc cmd) in
           '(("ec" "clear" flycheck-clear
              "eh" "describe" flycheck-describe-checker
              "el" "toggle" jw/toggle-flycheck-error-list
              "eL" "error-list" jw/goto-flycheck-error-list
              "ee" "explain-error-at-point" flycheck-explain-error-at-point
              "es" "select" flycheck-select-checker
              "eS" "set-checker-executable" flycheck-set-checker-executable
              "ev" "verify-setup "flycheck-verify-setup))
           do (autoload cmd (expand-file-name "jw-funcs-error") jw-funcs-dir)
           do (define-key jw-leader-map (kbd key) (cons desc cmd)))

  (global-flycheck-mode))

(use-package hippie-exp
  :init
  (global-set-key (kbd "M-/") 'hippie-expand)
  ;; Disables "Using try-expand-dabbrev" on completions.
  (setq hippie-expand-verbose nil)
  (setq hippie-expand-try-functions-list
        '(
          ;; Try to expand word "dynamically", searching the current
          ;; buffer.
          try-expand-dabbrev
          ;; Try to expand word "dynamically", searching all other
          ;; buffers.
          try-expand-dabbrev-all-buffers
          ;; Try to expand word "dynamically", searching the kill
          ;; ring.
          try-expand-dabbrev-from-kill
          ;; Try to complete text as a file name, as many characters
          ;; as unique.
          try-complete-file-name-partially
          ;; Try to complete text as a file name.
          try-complete-file-name
          ;; Try to expand word before point according to all abbrev
          ;; tables.
          try-expand-all-abbrevs
          ;; Try to complete the current line to an entire line in the
          ;; buffer.
          try-expand-list
          ;; Try to complete the current line to an entire line in the
          ;; buffer.
          try-expand-line
          ;; Try to complete as an Emacs Lisp symbol, as many
          ;; characters as unique.
          try-complete-lisp-symbol-partially
          ;; Try to complete word as an Emacs Lisp symbol.
          try-complete-lisp-symbol)))

(provide 'jw-core-autocomplete)
;;; jw-core-autocomplete.el ends here
