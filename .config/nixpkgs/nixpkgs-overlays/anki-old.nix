self: super: {}
#{
  #anki = (import (fetchTarball https://github.com/NixOS/nixpkgs-channels/archive/nixos-18.09.tar.gz) {}).anki;
#}
# self: super:
# let pinnedPkgs = import (builtins.fetchGit {
#   name = "nixos-unstable-2018-10-09";
#   url = https://github.com/nixos/nixpkgs;
#   rev = "141eb8b84367ee481065c3c59e3f3cb098504f00";
#   # sha256 = "1kzncdzd7ky8h8rbw55d0apl9n4n6s59771f1gy43az33xwhkrsg";
# }) {};
# in {
#   anki = pinnedPkgs.anki;
# }
