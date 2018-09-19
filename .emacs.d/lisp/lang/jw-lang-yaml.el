;;; jw-lang-yaml --- YAML

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package yaml-mode
  :ensure
  :mode "\\.yaml\\'")

(provide 'jw-lang-yaml)
;;; jw-lang-yaml.el ends here
