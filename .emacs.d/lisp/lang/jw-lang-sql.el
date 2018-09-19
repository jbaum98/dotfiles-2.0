;;; jw-lang-sql --- SQL

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(add-hook 'sql-interactive-mode-hook
          (lambda ()
            (setq-default sql-prompt-regexp "^[_[:alpha:]]*[=][#>] "
                          sql-prompt-cont-regexp "^[_[:alpha:]]*[-][#>] ")))

(provide 'jw-lang-sql)
;;; jw-lang-sql.el ends here
