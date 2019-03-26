self: super:
{
  emacsPackagesNg = super.emacsPackagesNg.overrideScope'(selfEpkgs: superEpkgs: {
    merlin = superEpkgs.merlin.overrideAttrs (oldAttrs: {
      meta = oldAttrs.meta // { broken = false; };
    });

    utop = superEpkgs.utop.overrideAttrs (oldAttrs: {
      meta = oldAttrs.meta // { broken = false; };
    });

    ocp-indent = superEpkgs.ocp-indent.overrideAttrs (oldAttrs : {
      version = "20180417.1549";
      src = super.fetchFromGitHub {
        owner = "OCamlPro";
        repo = "ocp-indent";
        rev = "95b83413ef449fefa6425249f1617b98425a0827";
        sha256 = "1ybvskw58qnxvlz54q911sxbkn1yjrss1hr5dkgms1khrl4q04sj";
      };
      recipe = super.fetchurl {
        url = "https://raw.githubusercontent.com/milkypostman/melpa/a1ff188d509aec104e9d21a640cf5bc3addedf00/recipes/ocp-indent";
        sha256 = "0wc4z9dsnnyr24n3vg1npvc3rm53av8bpbvrl8kldxxdiwgnbkjw";
        name = "recipe";
      };
      meta = oldAttrs.meta // { broken = false; };
    });

    flycheck-ocaml = (superEpkgs.flycheck-ocaml
      .override { inherit (self) merlin; })
      .overrideAttrs (oldAttrs: {
        meta = oldAttrs.meta // { broken = false; };
      });
  });
}
