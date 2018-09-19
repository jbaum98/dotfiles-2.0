;;; jw-core-Magit --- Magit  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package magit
  :ensure
  :config
  (setq magit-completing-read-function 'ivy-completing-read)
  :mode ("COMMIT_EDITMSG" . git-commit-mode)
  :bind
  (:map jw-leader-map
        ("gs" . magit-status)))

(provide 'jw-core-magit)
;;; jw-core-magit.el ends here
