;;; jw-core-autocomplete.el --- Config for autocompletion

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package company
  :ensure
  :defer 1
  :commands company-mode
  :bind
  (:map company-mode-map
   ("M-/" . company-complete))
  (:map company-active-map
   ("M-n" . company-select-next)
   ("M-p" . company-select-previous)
   ("C-j" . company-select-next)
   ("C-k" . company-select-previous)
   ("TAB" . company-complete-selection))
  :config
  (setq
   company-idle-delay 0
   company-show-numbers t
   tab-always-indent 'complete
   company-backends
   '((company-tabnine        ; super smart
      company-files          ; files & directory
      company-keywords       ; keywords
      company-capf)
     (company-abbrev company-dabbrev))))

(use-package company-tng
  :after company
  :config
  (company-tng-configure-default))

(use-package company-tabnine
  :ensure
  :pin melpa
  :commands company-tabnine)

(defun jw--company-sort-tabnine-first (candidates)
  (cl-flet ((backend (candidate)
                     (or (get-text-property 0 'company-backend candidate)
                         (cl-some (lambda (x) (and (not (keywordp x)) x))
                                  company-backend))))
    (sort candidates (lambda (e1 e2)
                       (and (eq (backend e1) 'company-tabnine)
                            (not (eq (backend e2) 'company-tabnine)))))))

(provide 'jw-core-autocomplete)
;;; jw-core-autocomplete.el ends here
