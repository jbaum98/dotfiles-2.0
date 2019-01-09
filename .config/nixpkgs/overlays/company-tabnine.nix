self: super:
{
  tabnine = super.stdenv.mkDerivation rec {
    name = "TabNine-${version}";
    version = "1.0.10";

    src = super.fetchurl {
      url =
        let target = super.stdenv.targetPlatform.config;
        in "https://update.tabnine.com/${version}/${target}/TabNine";
      sha256 = "1sjn0m9i1wij5jqk2c6lhh1arh4vsz0l99q3dbw99qgk8rivikva";
    };

    phases = ["installPhase"];

    installPhase = ''
      mkdir -p $out/bin
      cp $src $out/bin/TabNine
      chmod +x $out/bin/TabNine
      '';

  };

  emacsPackagesNg = super.emacsPackagesNg // {
    melpaPackages = super.emacsPackagesNg.melpaPackages // {
      company-tabnine = super.emacsPackagesNg.callPackage (
        { cl-lib ? null
        , company
        , emacs
        , fetchFromGitHub
        , fetchurl
        , lib
        , melpaBuild
        , s
        , unicode-escape }:
        melpaBuild rec {
          pname = "company-tabnine";
          ename = "company-tabnine";
          version = "20181207.2331";

          src = fetchFromGitHub {
            owner = "TommyX12";
            repo = "company-tabnine";
            rev = "2d63df791027ec2bcc8956be6b7078d17f95217c";
            sha256 = "06p7z0nnal26xb3kkh3ik0q42wkn146mr15bz3c1amfpkx60y1qi";
          };

          recipe = fetchurl {
            url = "https://raw.githubusercontent.com/melpa/melpa/94476897a71a271b985967334632836252eb131b/recipes/company-tabnine";
            sha256 = "1x37xacrscmh9hq9mljbgdcl3pwfn2kmn567qv0jqys8ihbzi3v7";
            name = "recipe";
          };

          packageRequires = [ cl-lib company emacs s unicode-escape ];

          patches = [ ./company-tabnine.patch ];
          postPatch = ''
            substituteInPlace company-tabnine.el \
              --subst-var-by tabnine "${self.tabnine}/bin/TabNine"
          '';

          meta = {
            homepage = "https://melpa.org/#/company-tabnine";
            license = lib.licenses.free;
          };
        }) {};

    };
  };
}
