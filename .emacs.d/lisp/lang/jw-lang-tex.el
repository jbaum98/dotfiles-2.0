;;; jw-lang-tex --- TeX

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package tex-site
  :ensure auctex
  :mode ("\\.tex\\'" . TeX-mode))

(provide 'jw-lang-tex)
;;; jw-lang-tex.el ends here
