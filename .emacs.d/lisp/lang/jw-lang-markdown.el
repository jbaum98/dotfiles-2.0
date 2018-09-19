;;; jw-lang-markdown --- Markdown

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package markdown-mode
  :ensure
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (setq-default markdown-command "multimarkdown"))

(provide 'jw-lang-markdown)
;;; jw-lang-markdown.el ends here
