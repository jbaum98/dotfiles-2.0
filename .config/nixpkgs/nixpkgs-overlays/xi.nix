self: super:
let
  version = "0da3c005a9dc309011774dac6b44c40553e24832";
  dtzWill-nur = import (builtins.fetchTarball "https://github.com/dtzWill/nur-packages/archive/${version}.tar.gz") {};
in {
  inherit (dtzWill-nur.pkgs) xi-core xi-term gxi;
}
