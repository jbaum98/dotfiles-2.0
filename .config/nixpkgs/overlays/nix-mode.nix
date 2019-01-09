self: super:
{
  emacsPackagesNg = super.emacsPackagesNg // {
    melpaPackages = super.emacsPackagesNg.melpaPackages // {
      nix-company = super.emacsPackagesNg.callPackage (
      { emacs , fetchFromGitHub , fetchurl , lib , melpaBuild }:
      melpaBuild {
        pname = "nix-company";
        ename = "nix-company";
        version = "20190108.147";
        src = fetchFromGitHub {
          owner = "NixOS";
          repo = "nix-mode";
          rev = "54ef83310095689443c2371a312cc8687af6cbb9";
          sha256 = "1n8nn3zq8skdb1q7vg5na7lfm2cf1hf8b2h8lmif27qz14lqsilr";
        };
        recipe = fetchurl {
          url = "https://raw.githubusercontent.com/jbaum98/melpa/9cc2ff9e1731c975cc4f614f36b9466a8f3a0e06/recipes/nix-company";
          sha256 = "1flkb57rzmx0pp9wpyld1hj1s5sndf9nymrd1mzpz3y6094l65sp";
          name = "recipe";
        };
        packageRequires = [ emacs ];
        meta = {
          homepage = "https://melpa.org/#/nix-mode";
          license = lib.licenses.free;
        };
      }) {};

      nix-mode = super.emacsPackagesNg.callPackage (
      { emacs , fetchFromGitHub , fetchurl , lib , melpaBuild }:
      melpaBuild {
        pname = "nix-mode";
        ename = "nix-mode";
        version = "20190108.147";
        src = fetchFromGitHub {
          owner = "NixOS";
          repo = "nix-mode";
          rev = "54ef83310095689443c2371a312cc8687af6cbb9";
          sha256 = "1n8nn3zq8skdb1q7vg5na7lfm2cf1hf8b2h8lmif27qz14lqsilr";
        };
        recipe = fetchurl {
        url = "https://raw.githubusercontent.com/melpa/melpa/e1870d786dbfac3b14386c8030e06f2d13ab9da6/recipes/nix-mode";
          sha256 = "10f3ly4860lkxzykw4fbvhn3i0c2hgj77jfjbhlk2c1jz9x4yyy5";
          name = "recipe";
        };
        packageRequires = [ emacs ];
        meta = {
          homepage = "https://melpa.org/#/nix-mode";
          license = lib.licenses.free;
        };
      }) {};
    };
  };
}
