;;; jw-lang-elm --- Elm

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package elm-mode
  :ensure
  :mode "\\.elm\\'"
  :hook (elm-mode . (lambda () (progn
                                 (require 'flycheck-elm)
                                 (add-to-list 'flycheck-checkers 'elm))))
  :config
  (setq elm-format-on-save t))

(use-package flycheck-elm
  :ensure
  :defer t)

(provide 'jw-lang-elm)
;;; jw-lang-elm.el ends here
