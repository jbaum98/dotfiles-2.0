;;; jw-core-xdg.el --- Setup XDG directories

;;; Commentary:

;;; Code:


;; Setup XDG directory
(cl-macrolet
    ((getdir (variable fallback)
             `(expand-file-name (or (getenv ,variable) ,fallback))))
  (defvar user-emacs-config-directory (getdir "XDG_CONFIG_HOME" "~/.config/"))
  (defvar user-emacs-data-directory (getdir "XDG_DATA_HOME" "~/.local/share/"))
  (defvar user-emacs-cache-directory (getdir "XDG_CACHE_HOME" "~/.cache/")))

(provide 'jw-core-xdg)
;;; jw-core-xdg.el ends here
