;;; jw-lang-java --- Java

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package meghanada
  :ensure
  :hook (java-mode-hook . meghanada-mode))

(provide 'jw-lang-java)
;;; jw-lang-java.el ends here
