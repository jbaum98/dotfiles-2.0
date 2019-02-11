;;; jw-core-check.el --- Config for automatically checking code

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package)
  (require 'general))

(use-package flycheck
  :custom (flycheck-emacs-lisp-load-path 'inherit)
  :commands flycheck-mode
  ;; Fix keybinding issues
  :hook (flycheck-mode . evil-normalize-keymaps)
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
    :fringe-face 'flycheck-fringe-info))

(use-package jw-funcs-error
  :general
  (jw-leader-def flycheck-mode-map
    :infix "e"
    "c" '(flycheck-clear :wd "clear")
    "h" '(flycheck-describe-checker :wd "describe")
    "l" '(jw/toggle-flycheck-error-list :wd "toggle")
    "L" '(jw/goto-flycheck-error-list :wd "error-list")
    "e" '(flycheck-explain-error-at-point :wd "explain-error-at-point")
    "s" '(flycheck-select-checker :wd "select")
    "S" '(flycheck-set-checker-executable :wd "set-checker-executable")
    "v" '(flycheck-verify-setup :wd "verify-setup")))

(provide 'jw-core-check)
;;; jw-core-check.el ends here
