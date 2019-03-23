;;; jw-core-dash.el --- Config for browsing dash config

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package)
  (use-package jw-core-xdg :load-path "lisp/core"))


(use-package helm-dash
  :config
  ;; (setq helm-dash-docsets-path (expand-file-name "docsets" user-emacs-cache-directory)
  ;;       helm-dash-browser-func 'eww)
  )

(provide 'jw-core-dash)
;;; jw-core-dash.el ends here
