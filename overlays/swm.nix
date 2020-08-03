self: super:

let
  version = "0.4.0-alpha1";

  swm-src = builtins.fetchTarball {
    url = "https://github.com/kalbasit/swm/archive/v${version}.tar.gz";
    sha256 = "17s6ccwg4240b6ykam27pz4kisyfm977rh1zzzx6ykarqi0x2fkf";
  };
in {
  swm = super.shabka.external.nixpkgs.release-unstable.callPackage "${swm-src}/default.nix" { inherit version; };
}
