(eval-when-compile
  (declare-function use-package-process-keywords "use-package" (name-symbol rest state)))

(defun use-package-nil-handler (name-symbol keyword ensure rest state)
  (let* ((body (use-package-process-keywords name-symbol rest state)))
    body))

(with-eval-after-load "use-package"
  (advice-add 'use-package-handler/:pin :override 'use-package-nil-handler)
  (advice-add 'use-package-handler/:ensure :override 'use-package-nil-handler))
