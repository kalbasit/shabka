self: super:

let
  version = "0.4.0-alpha3";

  swm-src = builtins.fetchTarball {
    url = "https://github.com/kalbasit/swm/archive/v${version}.tar.gz";
    sha256 = "1bghnf9w7wzbl2y5qg26hx48367iwlplqnlhw1hrnf0nh0yi5wr1";
  };
in {
  swm = super.shabka.external.nixpkgs.release-unstable.callPackage "${swm-src}/default.nix" { inherit version; };
}
