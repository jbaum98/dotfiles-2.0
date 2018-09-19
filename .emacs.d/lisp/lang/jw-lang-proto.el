;;; jw-lang-proto --- Proto

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))
(require 'jw-core-keybindings)

(use-package protobuf-mode
  :ensure
  :mode "\\.proto\\'")

(provide 'jw-lang-proto)
;;; jw-lang-proto.el ends here
