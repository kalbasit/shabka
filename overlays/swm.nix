self: super:

let
  version = "0.4.0-alpha2";

  swm-src = builtins.fetchTarball {
    url = "https://github.com/kalbasit/swm/archive/v${version}.tar.gz";
    sha256 = "0iwgccsxgx1fxv90vvwz73bmh65ik7i6qc7hmw1rwhh7830m0agp";
  };
in {
  swm = super.shabka.external.nixpkgs.release-unstable.callPackage "${swm-src}/default.nix" { inherit version; };
}
