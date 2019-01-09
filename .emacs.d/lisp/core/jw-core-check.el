;;; jw-core-check.el --- Config for automatically checking code

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

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

(provide 'jw-core-check)
;;; jw-core-check.el ends here
