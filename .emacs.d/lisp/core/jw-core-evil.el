;;; jw-core-evil.el --- Emacs is a great operating system...  -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package evil
  :ensure
  :commands evil-mode evil-define-key*
  :demand
  :init
  (setf
   ;; Fix coq expansion bug https://github.com/ProofGeneral/PG/issues/174
   evil-want-abbrev-expand-on-insert-exit nil
   ;; Use visual movements
   evil-respect-visual-line-mode t)
  :config
  (evil-mode 1)

  ;; Prevents esc-key from translating to meta-key in terminal mode.
  (setq evil-esc-delay 0)

  ;; It's better that the default value is too small than too big.
  (setq-default evil-shift-width 2)

  ;; * and # search using symbols.
  (setq-default evil-symbol-word-search t)

  ;; Automatically make searches global
  (setq evil-ex-substitute-global t)

  ;; evil-want-Y-yank-to-eol must be set via customize to have an effect.
  (customize-set-variable 'evil-want-Y-yank-to-eol t)

  ;; Controls position of the mode line tag for the current mode,
  ;; e.g. <N>, <I>, etc.  Before places it before the major-mode.
  (setq evil-mode-line-format 'before)

  ;; http://emacs.stackexchange.com/questions/14940
  (fset 'evil-visual-update-x-selection 'ignore)

  ;; Don't use evil mode sometimes
  (add-to-list 'evil-emacs-state-modes 'flycheck-error-list-mode)
  (add-to-list 'evil-emacs-state-modes 'git-commit-mode))

;; Shows number of matches in mode-line when searching with evil.
(use-package evil-anzu
  :ensure
  ;; Lazy loading doesn't make a much sense because evil-anzu
  ;; only defines four defadvices for `evil-search' `evil-ex'
  ;; `evil-flash' `evil-ex'
  :demand)

;; Motions and text objects for delimited arguments, e.g. the params
;; in `def func(foo, bar, baz)'.
(use-package evil-args
  :ensure
  :bind
  (:map evil-inner-text-objects-map
   ("a" . evil-inner-arg)
   :map evil-outer-text-objects-map
   ("a" . evil-outer-arg)))

;; Enables two char keypress to exit most modes.
(use-package evil-escape
  :ensure
  :diminish evil-escape-mode
  :hook (pre-command . evil-escape-pre-command-hook)
  :init
  (setf evil-escape-key-sequence "fd"
        evil-escape-unordered-key-sequence t))

;; Parenthesize easily
(use-package evil-surround
  :ensure
  :commands
  global-evil-surround-mode
  evil-surround-edit
  evil-Surround-edit
  evil-surround-region
  evil-Surround-region
  evil-surround-delete
  evil-surround-change
  :config
  (global-evil-surround-mode 1)
  (evil-define-key 'operator global-map "s" 'evil-surround-edit)
  (evil-define-key 'operator global-map "S" 'evil-Surround-edit)
  (evil-define-key 'visual global-map "S" 'evil-surround-region)
  (evil-define-key 'visual global-map "gS" 'evil-Surround-region))

(provide 'jw-core-evil)
;;; jw-core-evil.el ends here
