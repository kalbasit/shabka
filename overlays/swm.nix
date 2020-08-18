self: super:

let
  version = "0.4.0-alpha4";

  swm-src = builtins.fetchTarball {
    url = "https://github.com/kalbasit/swm/archive/v${version}.tar.gz";
    sha256 = "05rym9i95ng6nij5j3228za7qmc76jdlmaxzsjjjk6ljpz7m55k2";
  };
in {
  swm = super.shabka.external.nixpkgs.release-unstable.callPackage "${swm-src}/default.nix" { inherit version; };
}
