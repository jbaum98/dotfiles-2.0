;;; jw-lang-haskell --- Haskell

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package haskell-mode
  :ensure
  :mode "\\.hs\\'"
  :interpreter "ghci")

(provide 'jw-lang-haskell)
;;; jw-lang-haskell.el ends here
