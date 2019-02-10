;;; init.el -- Get the ball rolling  -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

;; Make startup faster by reducing the frequency of garbage
;; collection.  The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

;; Let 'em know who we are.
(setf user-full-name "Jake Waksbaum"
      user-mail-address "jake.waksbaum@gmail.com")

;; Let us know how fast our startup us
;; Use a hook so the message doesn't get clobbered by other messages.
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))


;; Stop package.el from getting in the way
(setf
 ;; don't auto-initialize!
 package-enable-at-startup nil
 ;; don't add that `custom-set-variables' block to my init!
 package--init-file-ensured t)

;; Setup use-package
;;(eval-and-compile (require 'use-package))
;;
;;(use-package use-package
;;  :commands use-package
;;  :config
;;  (setf use-package-expand-minimally t))

;; Bring in the rest of our config
;; This uses byte-compiling magic to ensure that this path is
;; computed relative to the location at compile-time.
(eval-and-compile
  (defconst jw-lisp-dir (eval-and-compile
                          (if load-file-name
                              (expand-file-name "lisp" (file-name-directory load-file-name))
                            (expand-file-name "lisp"))))
  (defconst jw-core-dir  (expand-file-name "core"  jw-lisp-dir))
  (defconst jw-funcs-dir (expand-file-name "funcs" jw-lisp-dir))
  (defconst jw-lang-dir  (expand-file-name "lang"  jw-lisp-dir))

  (add-to-list 'load-path jw-core-dir)
  (add-to-list 'load-path jw-lang-dir)
  (add-to-list 'load-path jw-funcs-dir))

(require 'jw-core-emacs-settings)
(require 'jw-core-ui)
(require 'jw-core-evil)
(require 'jw-core-keybindings)
(require 'jw-core-org)
(require 'jw-core-utils)
(require 'jw-core-autocomplete)
(require 'jw-core-check)
(require 'jw-core-ivy)
(require 'jw-core-magit)

;;(require 'jw-lang-aurora)
;;(require 'jw-lang-coq)
;;(require 'jw-lang-tex)
;;(require 'jw-lang-elisp)
;;(require 'jw-lang-elm)
;;(require 'jw-lang-go)
;;(require 'jw-lang-haskell)
;;(require 'jw-lang-java)
;;(require 'jw-lang-lua)
;;(require 'jw-lang-markdown)
;;(require 'jw-lang-nix)
;;(require 'jw-lang-ocaml)
;;(require 'jw-lang-python)
;;(require 'jw-lang-pdf)
;;(require 'jw-lang-proto)
;;(require 'jw-lang-rust)
;;(require 'jw-lang-sql)
;;(require 'jw-lang-yaml)

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))

(provide 'init)
;;; init.el ends here
