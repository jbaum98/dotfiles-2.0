;;; jw-core-autocomplete.el --- Config for autocompletion

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package)
  (require 'general))

(use-package general :commands general-define-key)

(make-variable-buffer-local 'company-backends)
(make-variable-buffer-local 'company-transformers)

(use-package company
  :commands company-mode
  :general
  (general-def company-mode-map
   "M-/" 'company-complete)
  (general-def company-active-map
   "M-n" 'company-select-next
   "M-p" 'company-select-previous
   "C-j" 'company-select-next
   "C-k" 'company-select-previous
   "TAB" 'company-complete-selection)
  :config
  (setq-default
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
  ;; TODO: use :functions
  :commands company-tng-configure-default
  :config
  (company-tng-configure-default))

(use-package company-tabnine
  :commands company-tabnine)

(defun jw--company-sort-tabnine-first (candidates)
  (cl-flet ((backend (candidate)
                     (get-text-property 0 'company-backend candidate)))
    (sort candidates (lambda (e1 e2)
                       (and (eq (backend e1) 'company-tabnine)
                            (not (eq (backend e2) 'company-tabnine)))))))

(provide 'jw-core-autocomplete)
;;; jw-core-autocomplete.el ends here
