self: super:
{
  emacs = super.emacsPackagesNg.emacsWithPackages (epkgs:
    (with epkgs; [
      cdlatex
      company
      company-coq
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
      company-tabnine
      flycheck
      nix-mode
      use-package
    ]) ++ (with epkgs.elpaPackages; [
      auctex
    ]));
}
