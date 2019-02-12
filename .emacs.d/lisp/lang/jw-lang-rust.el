;;; jw-lang-rust --- Rust

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package rust-mode :mode "\\.rs\\'")

(use-package cargo
  :hook (rust-mode . cargo-minor-mode))

(use-package racer
  :hook ((rust-mode . racer-mode)
         (rust-mode . eldoc-mode)
         (rust-mode . company-mode)))

(use-package flycheck-rust
  :hook (flycheck-mode . flycheck-rust-setup))

(provide 'jw-lang-rust)
;;; jw-lang-rust.el ends here
