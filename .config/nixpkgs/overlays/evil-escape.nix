# there's a bug in the current source of evil-escape that causes it to
# fail to build. We'll patch it out for now and hope it gets fixed in a
# future version.
self: super:
{
  emacsPackagesNg = super.emacsPackagesNg.overrideScope' (selfEpkgs: superEpkgs: {
    evil-escape = superEpkgs.evil-escape.overrideAttrs(attrs: {
      patches = (attrs.patches or []) ++ [
        (super.fetchpatch {
          url = https://github.com/BrianHicks/evil-escape/commit/b548e8450570a0c8dea47b47221b728c047a9baf.patch;
          sha256 = "1a2qrf4bpj7wm84qa3haqdg3pd9d8nh5vrj8v1sc0j1a9jifsbf6";
        })
      ];
    });
  });
}
