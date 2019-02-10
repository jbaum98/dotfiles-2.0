{ nixpkgs ? import <nixpkgs> {}}:
with nixpkgs;
with callPackage ./lib.nix {};
let
  emacsd = builtins.path { name = "emacsd"; path = "/home/jakewaksbaum/.emacs.d"; };
    extracted = extractPackageNames "${emacsd}/init.el";
  #extracted = { elpa = [ "spinner" "auctex"]; unpinned = ["esup" "evil-escape"]; };

  customEmacsPackages =
    emacsPackagesNg.overrideScope' (makeOverlay extracted);
in customEmacsPackages.emacsWithPackages (epkgs:
     indexNames (allNames extracted) epkgs)
