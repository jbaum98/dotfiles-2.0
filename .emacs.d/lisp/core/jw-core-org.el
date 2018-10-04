;;; jw-core-org --- Org mode

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(eval-when-compile
  (autoload 'org-bookmark-jump-unhide "org"))

(use-package org
  :mode  "\\.org\'"
  :commands org-bookmark-jump-unhide
  :init
  (setq-default org-list-allow-alphabetical t)
  :config
  (setq-default org-src-fontify-natively t
                org-src-tab-acts-natively t
                org-src-window-setup 'reorganize-frame))

(use-package jw-funcs-org
  :after ox
  :commands jw--filter-plain-list
  :init
  (add-to-list 'org-export-filter-plain-list-functions
               'jw--filter-plain-list))

(use-package org-bullets
  :ensure
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("∙")))


(use-package htmlize
  :ensure
  :commands
  htmlize-buffer
  htlmize-file
  htlmize-many-files)

(use-package cdlatex
  :ensure
  ;; org will require it
  :defer)

(use-package auctex
  :pin elpa
  :ensure
  ;; org will require it
  :defer)

(use-package org-ref
  :no-require
  :ensure
  :defer)

(provide 'jw-core-org)
;;; jw-core-org.el ends here
