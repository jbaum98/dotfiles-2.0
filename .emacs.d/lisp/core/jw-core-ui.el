;;; jw-core-ui --- Make it look nice  -*- lexical-binding: t; -*-

;;; Commentary:

;; Here we make Emacs look the way we want. Everything in this file
;; will be loaded on demand, which means there should be very few
;; packages here.

;;; Code:

(eval-when-compile
  (require 'use-package))

(defvar after-make-console-frame-hooks '()
  "Hooks to run after creating a new TTY frame.")

(defvar after-make-window-system-frame-hooks '()
  "Hooks to run after creating a new window system frame.")

(defun run-after-make-frame-hooks (frame)
  "Run configured hooks in response to the newly-created FRAME.
Selectively runs either `after-make-console-frame-hooks' or
`after-make-window-system-frame-hooks'"
  (with-selected-frame frame
    (run-hooks (if window-system
                   'after-make-window-system-frame-hooks
                 'after-make-console-frame-hooks))))

(add-hook 'after-make-frame-functions 'run-after-make-frame-hooks)

(defconst jw/initial-frame (selected-frame)
  "The frame (if any) active during Emacs initialization.")

(add-hook 'after-init-hook
          (lambda () (when jw/initial-frame
                  (run-after-make-frame-hooks jw/initial-frame))))

;; Enable the mouse even in the terminal.
(add-hook 'after-make-console-frame-hooks 'xterm-mouse-mode)
(add-hook 'after-make-window-system-frame-hooks
          (lambda ()
            (tool-bar-mode -1)
            (scroll-bar-mode -1)
            (blink-cursor-mode -1)
            (menu-bar-mode -1)))
;; Set default font
(set-face-attribute 'default nil
                    :family "Iosevka"
                    :height 100
                    :weight 'normal
                    :width 'normal)

;; Go straight to the scratch buffer.
(setf inhibit-splash-screen t
      initial-scratch-message "")

;; Control over modes displayed in the modeline.
;;(use-package dim
;;  :ensure
;;  :demand
;;  :config
;;  (dim-major-names
;;   '((emacs-lisp-mode          "ELisp")
;;     (lisp-interaction-mode    "ELisp")
;;     (inferior-emacs-lisp-mode "ELisp>"))))

(setq-default mode-line-format (list
               ;; the buffer name; the file name as a tool tip
               '(:eval (propertize "%b " 'face 'font-lock-keyword-face
                                   'help-echo (buffer-file-name)))

               ;; line and column
               "(" ;; '%02' to set to 2 chars at least; prevents flickering
               (propertize "%02l" 'face 'font-lock-type-face) ","
               (propertize "%02c" 'face 'font-lock-type-face)
               ") "

               ;; relative position, size of file
               "["
               (propertize "%p" 'face 'font-lock-constant-face) ;; % above top
               "/"
               (propertize "%I" 'face 'font-lock-constant-face) ;; size
               "] "

               ;; the current major mode for the buffer.
               "["

               '(:eval (propertize "%m" 'face 'font-lock-string-face
                                   'help-echo buffer-file-coding-system))
               "] "


               '(:eval (when (buffer-modified-p)
                         ((propertize "Mod "
                                      'face 'font-lock-warning-face
                                      'help-echo "Buffer has been modified"))))

               ;; is this buffer read-only?
               '(:eval (when buffer-read-only
                         (propertize "RO "
                                     'face 'font-lock-type-face
                                     'help-echo "Buffer is read-only")))

               ;; add the time, with the date and the emacs uptime in the tooltip
               '(:eval (propertize (format-time-string "%H:%M")
                                   'help-echo
                                   (concat (format-time-string "%c; ")
                                           (emacs-uptime "Uptime:%hh"))))

               '(:eval company-lighter)
               ))

;; Solarized is easy on the eyes.
(use-package solarized-theme
  :ensure
  :config (load-theme 'solarized-light t))

;; Display pretty symbols
(use-package prog-mode
  :defer 3
  :commands global-prettify-symbols-mode
  :config
  (setq-default prettify-symbols-unprettify-at-point 'right-edge)
  (global-prettify-symbols-mode))

(provide 'jw-core-ui)
;;; jw-core-ui.el ends here
