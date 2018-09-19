;;; jw-module-ivy.el --- Ivy setup

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))
(require 'jw-core-keybindings)

(use-package counsel
  :ensure
  :commands counsel-mode
  :diminish counsel-mode
  :bind
  (;; Current global keymap
   ("M-x" . counsel-M-x)

   :map jw-leader-map
   ("SPC" . counsel-M-x)

   ;; files
   ("fel" . counsel-find-library)
   ("ff" . counsel-find-file)
   ("fL" . counsel-locate)
   ("fr" . counsel-recentf)

   ;; help
   ("?"  . counsel-descbinds)
   ("hdf" . counsel-describe-function)
   ("hdm" . describe-mode)
   ("hdv" . counsel-describe-variable)
   ("hR" . spacemacs/counsel-search-docs)

   ;; register/ring
   ("ry" . counsel-yank-pop)

   ;; jumping
   ("sj" . counsel-imenu)
   ("ji" . counsel-imenu)

   ;; insert
   ("iu" . counsel-unicode-char)

   ;; search
   ("/"  . counsel-rg)
   ("sp"  . counsel-rg))
  :config
  ;; Remaps built-in commands that have a counsel replacement.
  (counsel-mode 1))

(use-package ivy
  :ensure
  :diminish ivy-mode
  :bind
  (:map jw-leader-map
   ("bb" . ivy-switch-buffer)
   ("rl" . ivy-resume)

   :map ivy-minibuffer-map
   ("C-j" . ivy-next-line)
   ("C-k" . ivy-previous-line)
   ("C-M-j" . ivy-scroll-up-command)
   ("C-M-k" . ivy-scroll-down-command)
   ("C-<return>" . ivy-alt-done)
   ("M-<return>" . ivy-immediate-done)
   ("C-M-n" . ivy-restrict-to-matches)
   ("C-h" . backward-delete-char-untabify)
   ("C-S-h" . help-map)
   ("C-l" . ivy-alt-done)
   ("<escape>" . minibuffer-keyboard-quit))

  :init
  ;; 15 lines in minibuffer
  (setq ivy-height 15)
  (setq ivy-count-format "(%d/%d)")
  ;; Recent files and bookmarks to `ivy-switch-buffer'
  (setq ivy-use-virtual-buffers t)
  ;; Don't show . and .. in find files.
  (setq ivy-extra-directories '())
  ;; Don't exit if we press backspace too many times.
  (setq ivy-on-del-error-function (lambda ()))
  (setq completing-read-function 'ivy-completing-read)
  (setq completion-in-region-function 'ivy-completion-in-region))

;; counsel-M-x will use smex if available.
(use-package smex
  :ensure
  :defer t
  :config
  (setq-default smex-save-file (expand-file-name "smex-items" user-emacs-directory)))

(use-package swiper
  :ensure
  :bind
  (;; Current global keymap.
   ("\C-s" . swiper)

   :map jw-leader-map
   ("ss" . swiper)
   ("sS" . spacemacs/swiper-region-or-symbol)
   ("sb" . swiper-all)
   ("sB" . spacemacs/swiper-all-region-or-symbol)))

(provide 'jw-core-ivy)
;;; jw-core-ivy.el ends here
