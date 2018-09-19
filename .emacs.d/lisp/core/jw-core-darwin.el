;;; jw-core-darwin.el --- customization for macOS

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(setq mac-option-modifier 'control
      mac-command-modifier 'meta)

(use-package browse-url
  :defer t
  :ensure nil ; built-in package
  :config
  (setq browse-url-generic-program "open"
        browse-url-browser-function 'browse-url-default-macosx-browser))


(provide 'jw-core-darwin)
;;; jw-core-darwin.el ends here
