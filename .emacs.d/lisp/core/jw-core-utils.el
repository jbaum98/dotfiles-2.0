;;; jw-core-utils --- Utilities

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package use-package
  :commands use-package
  :config
  (setf use-package-expand-minimally t))

(use-package esup
  :commands esup)

(provide 'jw-core-utils)
;;; jw-core-utils.el ends here
