;;; jw-lang-pdf --- PDF

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package pdf-tools
  :ensure
  :mode ("\\.pdf\\'" . doc-view-mode))

(provide 'jw-lang-pdf)
;;; jw-lang-pdf.el ends here
