;;; jw-core-emacs-settings.el --- Editor defaults

;;; Commentary:

;; Alphabetical order by identifier.

;;; Code:

(eval-when-compile
  (require 'use-package))

(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "auto-save-list" user-emacs-directory) t)))

;; Directory to store backup files.
(setq-default backup-directory-alist `(("." . ,(expand-file-name "backups" user-emacs-directory)))
              tramp-auto-save-directory (expand-file-name "tramp-auto-save" user-emacs-directory)
              tramp-backup-directory-alist backup-directory-alist
              url-configuration-directory (expand-file-name "url" user-emacs-directory)
              savehist-file (expand-file-name "history" user-emacs-directory))

;; Case insensitive search
(setq case-fold-search t)

;; Displays column number in the mode line.
(setq column-number-mode t)

;; Moves the custom file out of the bottom of init.el.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; Replace highlighted text with type.
(delete-selection-mode 1)

;; Delete trailing whitespace before saving.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Deletes excess backup versions silently.
(setq delete-old-versions t)

;; Explicitly set the prefered coding systems to avoid annoying prompt
;; from Emacs (especially on Microsoft Windows).
(set-charset-priority 'unicode)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(setq-default buffer-file-coding-system 'utf-8)

;; We don't share the file-system with anyone else.
(setq create-lockfiles nil)

;; Make all scripts executable by default
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; Never insert tabs.
(setq-default indent-tabs-mode nil)

;; Skip startup screen.
(setq inhibit-startup-screen t)

;; Start with a blank canvas.
(setq initial-scratch-message "")

;; Warns when opening files bigger than 10MB.
(setq large-file-warning-threshold (* 10 1024 1024))

;; Always use lexical bindings
(setq lexical-binding 't)

;; Load the newer .elc or .el file, rather than stopping at .elc.
(setq load-prefer-newer t)

(setq mouse-yank-at-point t)

;; Potentially speed up cursor operations
;; https://emacs.stackexchange.com/questions/28736
(setq auto-window-vscroll nil)

;; Too useful to disable
(put 'narrow-to-region 'disabled nil)

;; Disable emacs package manager
(setq-default package-enable-at-startup nil
              package--init-file-ensured t)

(defun jw--package-setup-melpa ()
  (let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                      (not (gnutls-available-p))))
         (proto (if no-ssl "http" "https")))
    (when no-ssl
      (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
    ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
    (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
    ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
    (when (< emacs-major-version 24)
      ;; For important compatibility libraries like cl-lib
      (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/"))))))

(use-package package
  :defer
  :config (jw--package-setup-melpa))

;; Newline at end of file.
(setq require-final-newline t)

;; Disables the annoying bell ring.
(setq ring-bell-function 'ignore)

;; Store pastes from other programs in the kill-ring before
;; overwriting with Emacs' yanks.
(setq save-interprogram-paste-before-kill t)

;; Save history and cursor position
(save-place-mode)
(add-hook 'after-init-hook (lambda () (savehist-mode 1)))

;; Highlight matching parentheses.
(show-paren-mode 1)

;; Automatically insert matching parentheses
(add-hook 'prog-mode-hook 'electric-pair-mode)

;; Remember recent files
(unless noninteractive (recentf-mode 1))

;; End a sentence with one period.
(setq sentence-end-double-space nil)

;; Enables nice scrolling.
(setq scroll-margin 0)
(setq scroll-conservatively 100000)
(setq scroll-preserve-screen-position 1)

;; Indentation at 2 spaces
(setq-default indent-tabs-mode nil)
(custom-set-variables
 '(tab-width 2)
 '(c-basic-offset 2))

;; Number backup files.
(setq version-control t)

;; Don't ask for confirmation when opening symlinked file.
(setq vc-follow-symlinks t)

;; Make backup files even when in version controlled directory.
(setq vc-make-backup-files t)

;; y is shorter than yes.
(fset 'yes-or-no-p 'y-or-n-p)

;; Load customizations from the custom file
(load custom-file t)

(provide 'jw-core-emacs-settings)
;;; jw-core-emacs-settings.el ends here
