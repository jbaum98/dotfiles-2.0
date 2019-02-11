self: super:
{
  emacs = super.emacsPackagesNg.emacsWithPackages (epkgs:
    (with epkgs; [
      cdlatex
      company
      company-coq
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
      magit
      merlin
      org-bullets
      org-ref
      proof-general
      smex
      solarized-theme
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
