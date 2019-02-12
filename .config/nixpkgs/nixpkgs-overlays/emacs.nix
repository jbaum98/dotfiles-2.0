self: super:
{
  emacs = super.emacsPackagesNg.emacsWithPackages (epkgs:
    (with epkgs; [
      cargo
      cdlatex
      company
      company-coq
      company-lsp
      company-tabnine
      counsel
      esup
      evil
      evil-anzu
      evil-args
      evil-escape
      evil-surround
      flycheck-rust
      general
      htmlize
      ivy
      lsp-mode
      lsp-ui
      magit
      merlin
      org-bullets
      org-ref
      projectile
      proof-general
      racer
      rust-mode
      smex
      solarized-theme
      spinner
      swiper
      tuareg
      which-key
      yaml-mode
    ]) ++ (with epkgs.melpaPackages; [
      flycheck
      nix-mode
      use-package
      proof-general
    ]) ++ (with epkgs.elpaPackages; [
      auctex
    ]));
}
