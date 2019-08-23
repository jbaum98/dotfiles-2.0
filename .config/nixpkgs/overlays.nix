[
  (import ./nixpkgs-overlays/emacs.nix)
  (import ./nixpkgs-overlays/anki-old.nix)
  (import ./nixpkgs-overlays/company-tabnine.nix)
  (import ./nixpkgs-overlays/clang-format.nix)
  (import ./nixpkgs-overlays/evil-escape.nix)
  (import ./nixpkgs-overlays/merlin.nix)
  (import ./nixpkgs-overlays/nix-mode.nix)
  (import ./nixpkgs-overlays/emacs-imagemagick.nix)
  (import ./nixpkgs-overlays/acsl-mode.nix)
  (import ./nixpkgs-overlays/emacs-pkgs.nix)
  (import ./nixpkgs-overlays/xi.nix)
  (import (builtins.fetchTarball https://github.com/mozilla/nixpkgs-mozilla/archive/master.tar.gz))
]
