self: super:
{
  emacsPackagesNg = super.emacsPackagesNg // {
    merlin = super.emacsPackagesNg.callPackage (
      { fetchFromGitHub, fetchurl, lib, melpaBuild }:
      melpaBuild {
        pname = "merlin";
        ename = "merlin";
        version = "20181212.316";
        src = fetchFromGitHub {
          owner = "ocaml";
          repo = "merlin";
          rev = "18f67a19dd340df8a7304c2b7b16c0baeb8e8117";
          sha256 = "1ck1h4d68px83l5vmm16p7qia7gcfxbi8ymc2w7y7an5f2158f7k";
        };
        recipe = fetchurl {
          url = "https://raw.githubusercontent.com/milkypostman/melpa/9338298a79b7f2d654df90b0f553aeed1428de13/recipes/merlin";
          sha256 = "0r4wc5ann6239bagj364yyzw4y3lcpkl5nnn0vmx4hgkwdg509fn";
          name = "recipe";
        };
        packageRequires = [];
        meta = {
          homepage = "https://melpa.org/#/merlin";
          license = lib.licenses.free;
        };
      }) {};

    flycheck-ocaml = (super.emacsPackagesNg.flycheck-ocaml
      .override { inherit (self.emacsPackagesNg) merlin; })
      .overrideAttrs (oldAttrs: { meta = oldAttrs.meta // { broken = false; }; });
  };
}
