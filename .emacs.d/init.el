;;; init.el -- Get the ball rolling  -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

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

;; Increase the ~gc-cons-threshold~ to avoid garbage collections
;; during startup.
(let ((gc-cons-threshold-backup gc-cons-threshold)
      (file-name-handler-alist-backup file-name-handler-alist))

  (setf gc-cons-threshold most-positive-fixnum
        file-name-handler-alist nil)

  (add-hook 'after-init-hook
            (lambda ()
              (garbage-collect)
              (setf gc-cons-threshold gc-cons-threshold-backup)
              file-name-handler-alist (append
                                       file-name-handler-alist-backup
                                       file-name-handler-alist))))

;; Stop package.el from getting in the way
(setf
 ;; don't auto-initialize!
 package-enable-at-startup nil
 ;; don't add that `custom-set-variables' block to my initl!
 package--init-file-ensured t)

;; Setup use-package the way we like.
(setf
 ;; let us find use-package declarations with imenu
 use-package-enable-imenu-support t
 ;; don't include debugging code in macro-expansion
 use-package-expand-minimally nil)

(eval-when-compile
  (require 'use-package))

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

(require 'jw-core-lib)
(require 'jw-core-emacs-settings)
(require 'jw-core-ui)
(require 'jw-core-keybindings)
(require 'jw-core-evil)
(when (bound-and-true-p IS-DARWIN)
  (require 'jw-core-darwin))
(require 'jw-core-autocomplete)

                                        ;(require 'jw-core-ivy)
                                        ;(require 'jw-core-org)
                                        ;(require 'jw-core-magit)

                                        ;(require 'jw-lang-aurora)
(require 'jw-lang-coq)
                                        ;(require 'jw-lang-elm)
                                        ;(require 'jw-lang-go)
                                        ;(require 'jw-lang-haskell)
                                        ;(require 'jw-lang-java)
                                        ;(require 'jw-lang-lua)
                                        ;(require 'jw-lang-markdown)
                                        ;(require 'jw-lang-nix)
                                        ;(require 'jw-lang-pdf)
                                        ;(require 'jw-lang-proto)
                                        ;(require 'jw-lang-rust)
                                        ;(require 'jw-lang-sql)
                                        ;(require 'jw-lang-yaml)

(use-package esup
  :ensure
  :commands esup)

(provide 'init)
;;; init.el ends here
