;;; jw-core-Magit --- Magit  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package)
  (require 'general))

(use-package magit
  :config
  (setq magit-completing-read-function 'ivy-completing-read)
  :mode ("COMMIT_EDITMSG" . git-commit-mode)
  :general
  (jw-leader-def "gs" 'magit-status))

(provide 'jw-core-magit)
;;; jw-core-magit.el ends here
