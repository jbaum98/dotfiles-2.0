;;; jw-lang-sql --- SQL

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package)
  (use-package jw-core-lib :load-path "lisp/core"))

(use-package sql
  :init
  (add-hook 'sql-interactive-mode-hook
            (jw/set-var-hook sql-prompt-regexp "^[_[:alpha:]]*[=][#>] "))
  (add-hook 'sql-interactive-mode-hook
            (jw/set-var-hook sql-prompt-cont-regexp "^[_[:alpha:]]*[-][#>] ")))

(provide 'jw-lang-sql)
;;; jw-lang-sql.el ends here
