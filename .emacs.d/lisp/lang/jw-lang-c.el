;;; jw-lang-c --- C

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package)
  (use-package jw-core-keybindings :load-path "lisp/core"))

(use-package general :commands general-define-key)

(use-package ggtags
  :hook (c-mode . ggtags-mode)
  :general
  (jw-leader-def c-mode-map
    :infix "j"
    "j" 'ggtags-find-tag-dwim))

(defun jw--acsl-font-lock-keywords ()
  "Load acsl-mode and return acsl-font-lock-keywords."
  (use-package acsl-mode)
  acsl-font-lock-keywords)

(use-package ccls)

(use-package lsp-mode :hook (c-mode . lsp))

;; (use-package acsl-mode
;;   :defer
;;   :init (add-hook 'c-mode-hook
;;                     (lambda () (font-lock-add-keywords nil (jw--acsl-font-lock-keywords)))))

;; (use-package flycheck
;;   :hook (c-mode . flycheck-mode))

;; https://eklitzke.org/smarter-emacs-clang-format
(defun jw--clang-format-buffer ()
  "Reformat buffer if .clang-format exists in the projectile root."
  (when (and (projectile-project-p)
             (file-exists-p (expand-file-name ".clang-format" (projectile-project-root))))
    (clang-format-buffer)))

(use-package clang-format
  :commands clang-format-buffer clang-format-region
  :init
  (add-hook
   'c-mode-hook
   (lambda () (add-hook 'before-save-hook 'jw--clang-format-buffer nil t))))

(provide 'jw-lang-c)
;;; jw-lang-c.el ends here
