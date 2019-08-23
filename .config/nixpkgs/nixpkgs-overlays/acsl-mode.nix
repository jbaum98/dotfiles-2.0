# there's a bug in the current source of evil-escape that causes it to
# fail to build. We'll patch it out for now and hope it gets fixed in a
# future version.
self: super:
{
  emacsPackagesNg = super.emacsPackagesNg.overrideScope' (selfEpkgs: superEpkgs: {
    acsl-mode = selfEpkgs.callPackage (
      { stdenv, fetchFromGitHub, writeTextFile, lib }:
      super.stdenv.mkDerivation rec {
        name = "acsl-mode";
        version = "18.0";

        src = fetchFromGitHub {
          owner = "Frama-C";
          repo = "Frama-C-snapshot";
          rev = version;
          sha256 = "sha256:0578zsdnnyyffbplllzk768gwxbbrs2akq6fa8097nv4rivkq5w4";
        };

        phases = [ "unpackPhase" "installPhase" ];

        installPhase = ''
          mkdir -p "$out/share/emacs/site-lisp"
          cp share/emacs/acsl.el  "$out/share/emacs/site-lisp/acsl-mode.el"
        '';

      }) {};
    });
}
