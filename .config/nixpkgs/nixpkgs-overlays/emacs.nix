self: super:
{
  emacs = super.emacsPackagesNg.emacsWithPackages (epkgs:
    (with epkgs; [
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
      smex
      solarized-theme
      spinner
      swiper
      tuareg
      which-key
    ]) ++ (with epkgs.melpaPackages; [
      flycheck
      nix-mode
      use-package
      proof-general
    ]) ++ (with epkgs.elpaPackages; [
      auctex
    ]));
}
