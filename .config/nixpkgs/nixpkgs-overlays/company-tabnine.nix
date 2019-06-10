self: super:
{
  tabnine = super.stdenv.mkDerivation rec {
    name = "TabNine-${version}";
    version = "1.0.10";

    src = super.fetchurl {
      url =
        let target = super.stdenv.targetPlatform.config;
        in "https://update.tabnine.com/${version}/${target}/TabNine";
      sha256 =
        if self.hostPlatform.isDarwin
        then "1k3rdrx98fjmbgxwn9y97ys6181x5ddjwy6qbh7dl9if5jgpxj7c"
        else "1sjn0m9i1wij5jqk2c6lhh1arh4vsz0l99q3dbw99qgk8rivikva";
    };

    phases = ["installPhase"];

    installPhase = ''
      mkdir -p $out/bin
      cp $src $out/bin/TabNine
      chmod +x $out/bin/TabNine
      '';

  };

  emacsPackagesNg = super.emacsPackagesNg.overrideScope' (selfEpkgs: superEpkgs: {
    company-tabnine = superEpkgs.melpaPackages.company-tabnine.overrideAttrs (oldAttrs: {
        patches = (oldAttrs.patches or []) ++ [ ./company-tabnine.patch ];
        postPatch = (oldAttrs.postPatch or "") + ''
          substituteInPlace company-tabnine.el \
            --subst-var-by tabnine "${self.tabnine}/bin/TabNine"
        '';
    });
  });
}
