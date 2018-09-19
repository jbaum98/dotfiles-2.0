;;; extract-packages --- Extract the names of ensured packages from use-package

;;; Commentary:

;;; Code:

(require 'use-package)

(defvar nix-package-alist '())

(defun nix-store-archive-in-state (args)
  "A wrapper around a use-package handler to store archive name in state plist.
Replaces new state in ARGS."
  (cl-destructuring-bind (name-symbol keyword archive-name rest state) args
    (let* ((state (plist-put state :archive-name (intern archive-name))))
      (list name-symbol keyword archive-name rest state))))

(defun nix-use-package-pass-handler (name keyword val rest state)
  "A wrapper around a use-package handler that only recurses.
Passes along NAME KEYWORD REST and STATE to `use-package-process-keywords'.
Ignores VAL."
  (use-package-process-keywords name rest state))

(defun nix-store-macro-package-in-alist (name-symbol keyword ensure rest state)
  "A wrapper around a use-package handler to store NAME-SYMBOL.
Stores NAME-SYMBOL under its archive-name in `nix-package-alist'.
Uses STATE to lookup the archive name.
Only stores the package if ENSURE is '(t) or (t . PKG-NAME).
Ignores KEYWORD and REST."
  (let* ((archive-name (or (plist-get state :archive-name) 'top-level))
         (pkg (or (and (equal ensure '(t)) name-symbol) (car ensure))))
    (when pkg
      ;; (message (format "%s:%s (%s)" name-symbol pkg ensure))
      `((cl-pushnew
         ',pkg
         (alist-get ',archive-name nix-package-alist))))))

;;; Wrap use-package-handler/:pin to store archive name in store for use-package-handler/:ensure
(advice-add 'use-package-handler/:pin :override 'nix-use-package-pass-handler)
(advice-add 'use-package-handler/:pin :filter-args 'nix-store-archive-in-state)
;;; Wrap use-package-handler/:ensure to generate no code
(advice-add 'use-package-handler/:ensure :override 'nix-store-macro-package-in-alist)

(defun output-packages (outdir)
  "Output the names of the packages for each archive in OUTDIR."
  (cd outdir)
  (dolist (archive-set nix-package-alist)
    (let ((archive-name (symbol-name (car archive-set)))
          (pkgs (cdr archive-set)))
      (with-temp-file archive-name
                      (dolist (pkg pkgs)
                        (insert (format "%s\n" pkg)))))))

(provide 'extract-packages)
;;; extract-packages.el ends here
