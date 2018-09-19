;;; jw-lang-nix --- Nix

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package nix-mode
  :ensure
  :mode "\\.nix\\'"
  :hook (nix-mode .
                  (lambda ()
                    (set (make-local-variable 'indent-line-function) 'nix-indent-line)))
  )

(use-package nix-shell
  :ensure nil
  :commands (nix-shell nix-unpack))

(use-package nix-buffer
  :no-require
  :ensure
  :commands nix-buffer
  :preface
  (defun turn-on-nix-buffer ()
    (when (and (not noninteractive)
               (not (eq (aref (buffer-name) 0) ?\s))
               (not (file-remote-p default-directory)))
      (nix-buffer)))
  :hook (after-change-major-mode . turn-on-nix-buffer))

(use-package nix-update
  :no-require
  :ensure nil
  :commands nix-update-fetch
  :bind (("C-. u" . nix-update-fetch)))

(provide 'jw-lang-nix)
;;; jw-lang-nix.el ends here
