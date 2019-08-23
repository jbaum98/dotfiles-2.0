self: super:
{
  emacsPackagesNg = super.emacsPackagesNg.overrideScope' (selfEpkgs: superEpkgs: {
    clang-format = superEpkgs.clang-format.overrideAttrs(attrs: {
      src = super.fetchFromGitHub {
        owner = "emacsmirror";
        repo = "clang-format";
        rev = "1469728c61dcba8fa09c456e841f97e9eb75fa85";
        sha256 = "0w6pd47pfs8jna076xjz0xz1f7bxdgvyglpllkm62fifiy2n994l";
      };
    });
  });
}
