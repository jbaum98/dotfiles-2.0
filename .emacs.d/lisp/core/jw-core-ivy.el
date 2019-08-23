;;; jw-module-ivy.el --- Ivy setup

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package)
  (use-package jw-core-keybindings :load-path "lisp/core"))

(use-package general :commands general-define-key)

(use-package counsel
  :defer 1
  :commands counsel-mode
  ;; :diminish counsel-mode
  :general
  (general-def "M-x" 'counsel-M-x)

  (jw-leader-def "SPC" 'counsel-M-x)

  (jw-leader-def
    :infix "f"
    "el" 'counsel-find-library
    "f" 'counsel-find-file
    "L" 'counsel-locate
    "r" 'counsel-recentf)

  (jw-leader-def "?" 'counsel-descbinds)
  (jw-leader-def
    :infix "h"
    "df" 'counsel-describe-function
    "dm" 'describe-mode
    "dv" 'counsel-describe-variable)

  (jw-leader-def
    "ry" 'counsel-yank-pop
    "sj" 'counsel-imenu
    "ji" 'counsel-imenu
    "iu" 'counsel-unicode-char
    "/" 'counsel-rg
    "sp" 'counsel-rg)

  :config
  ;; Remaps built-in commands that have a counsel replacement.
  (counsel-mode 1))

(use-package ivy
  ;; :diminish ivy-mode
  :commands ivy-completing-read ivy-completion-in-region
  :general
  (jw-leader-def
   "bb" 'ivy-switch-buffer
   "rl" 'ivy-resume)

  (general-def ivy-minibuffer-map
   "C-j" 'ivy-next-line
   "C-k" 'ivy-previous-line
   "C-M-j" 'ivy-scroll-up-command
   "C-M-k" 'ivy-scroll-down-command
   "C-<return>" 'ivy-alt-done
   "M-<return>" 'ivy-immediate-done
   "C-M-n" 'ivy-restrict-to-matches
   "C-h" 'backward-delete-char-untabify
   "C-S-h" 'help-map
   "C-l" 'ivy-alt-done
   "<escape>" 'minibuffer-keyboard-quit)

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
  :commands smex
  :config
  (setq-default smex-save-file (expand-file-name "smex-items" user-emacs-directory)))

(use-package swiper
  :general
  (general-def "\C-s" 'swiper)

  (jw-leader-def
    :infix "s"
   "ss" 'swiper
   "sb" 'swiper-all))

(provide 'jw-core-ivy)
;;; jw-core-ivy.el ends here
