self: super:

let
  version = "0.4.0-alpha";

  swm-src = builtins.fetchTarball {
    url = "https://github.com/kalbasit/swm/archive/v${version}.tar.gz";
    sha256 = "10ki0d74hddgmvknfny126c7b1pr0skhccrmzkqvcwszai6mnadn";
  };
in {
  swm = super.shabka.external.nixpkgs.release-unstable.callPackage "${swm-src}/default.nix" { inherit version; };
}
