;;; extract-packages --- Extract the names of ensured packages from use-package -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Jake Waksbaum

;; Author: Jake Waksbaum <jake.waksbaum@gmail.com>
;; Package-Requires: ((use-package "2.0"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Overrides the default behavior of use-package handlers to extract
;; the names of packages that are `:ensure'd. This allows us to use
;; some other mechanism for making those packages available to Emacs
;; (like nixpkgs).

;; This file should be used as follows:
;;
;; $ emacs --batch \
;;     --load extract-packages.el \
;;     --load init.el \
;;     --eval "(expkgs-output-packages $outdir)"
;;
;; Loading extract-packages.el will override the behavior of
;; use-package to record information about `:ensure'd packages and
;; instead of actually installing them. Then you load your
;; configuration to record which packages it `:ensure's. Finally you
;; call `output-packages' to create a directory with files whose names
;; are the symbols given to `:pin' and whose contents are the names of
;; the `:ensure'd packages. Unpinned packages are stored in a file
;; called `unpinned'.

;; We implement this functionality by overriding
;; `use-package-handler/:pin' to store the
;; archive name in the state plist under `:archive-name', and overriding
;; `use-package-handler/:ensure' to generate no code and add the
;; package to the `expkgs--alist' under the archive name in the state.

;;; Code:

(require 'use-package)

(defvar expkgs--alist '()
  "Associates archive symbols to the packages to be fetched from them.")

(defun expkgs--store-archive (args)
  "A wrapper around a use-package handler to store archive name in state plist.
Replaces state in ARGS."
  (cl-destructuring-bind (name-symbol keyword archive-name rest state) args
    (let* ((state (plist-put state :archive-name (intern archive-name))))
      (list name-symbol keyword archive-name rest state))))

(defun expkgs--pass-handler (name keyword val rest state)
  "A use-package handler that only recurses.
Passes along NAME REST and STATE to `use-package-process-keywords'.
Ignores KEYWORD and VAL."
  (ignore keyword val)
  (use-package-process-keywords name rest state))

(defun expkgs--write-to-alist (name-symbol keyword ensure rest state)
  "A wrapper around a use-package handler to store NAME-SYMBOL.
Stores NAME-SYMBOL under its archive-name in `expkgs--alist'.
Uses STATE to lookup the archive name, with `unpinned' as the default.
Only stores the package if ENSURE is '(t) or (t . PKG-NAME).
Ignores KEYWORD and REST."
  (ignore keyword rest)
  (let* ((archive-name (or (plist-get state :archive-name) 'unpinned))
         (pkg (or (and (equal ensure '(t)) name-symbol) (car ensure))))
    (when pkg
      `((cl-pushnew
         ',pkg
         (alist-get ',archive-name expkgs--alist))))))

;;; Wrap use-package-handler/:pin to store archive name in store for use-package-handler/:ensure
(advice-add 'use-package-handler/:pin :override 'expkgs--pass-handler)
(advice-add 'use-package-handler/:pin :filter-args 'expkgs--store-archive)
;;; Wrap use-package-handler/:ensure to write packages to `epkgs--alist'
(advice-add 'use-package-handler/:ensure :override 'expkgs--write-to-alist)

(defun expkgs-output-packages (outdir)
  "Output the names of the packages for each archive in OUTDIR."
  (cd outdir)
  (dolist (archive-set expkgs--alist)
    (let ((archive-name (symbol-name (car archive-set)))
          (pkgs (cdr archive-set)))
      (with-temp-file archive-name
        (dolist (pkg pkgs)
          (insert (format "%s\n" pkg)))))))

(provide 'extract-packages)
;;; extract-packages.el ends here
