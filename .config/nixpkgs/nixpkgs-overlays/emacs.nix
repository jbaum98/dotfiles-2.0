self: super:
let emacsWithPkgs = super.emacsPackagesNg.emacsWithPackages (epkgs:
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
    utop
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
in {
  emacs = super.runCommand emacsWithPkgs.name {
    buildInputs = [ super.makeWrapper ];
  }
  ''
    mkdir $out
    # Link every top-level folder from emacsWithPkgs to our new target
    ln -s ${emacsWithPkgs}/* $out
    # Except the bin folder
    rm $out/bin
    mkdir $out/bin
    # We create the bin folder ourselves and link every binary in it
    ln -s ${emacsWithPkgs}/bin/* $out/bin
    # Except the emcas binary
    rm $out/bin/emacs
    # Because we create this ourself, by creating a wrapper
    makeWrapper ${emacsWithPkgs}/bin/emacs $out/bin/emacs \
      --prefix PATH : ${super.ghostscript}/bin
  '';
}
