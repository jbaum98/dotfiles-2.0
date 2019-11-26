;;; jw-core-org --- Org mode

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package org
  :mode  "\\.org\'"
  :hook
  (org-mode . org-indent-mode)
  (org-mode . org-cdlatex-mode)
  (org-mode . (lambda () (electric-pair-local-mode -1)))
  :init
  (setq-default org-list-allow-alphabetical t)
  :config
  (setq-default org-src-fontify-natively t
                org-src-tab-acts-natively t
                org-src-window-setup 'reorganize-frame
                org-catch-invisible-edits 'show-and-error
                org-pretty-entities-include-sub-superscripts nil
                org-image-actual-width nil
                org-log-done t))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom  (org-bullets-bullet-list '("∙")))

(use-package htmlize
  :commands
  htmlize-buffer
  htlmize-file
  htlmize-many-files)

(use-package cdlatex
  ;; org will require it
  :defer)

(use-package auctex
  ;; org will require it
  :defer)

(use-package org-ref
  :commands org-ref)

(provide 'jw-core-org)
;;; jw-core-org.el ends here
