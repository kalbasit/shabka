{ pkgs, lib }:

let
  homeManager =
    let
      nixpkgs = import ../external/nixpkgs-stable.nix;
      pkgs = import nixpkgs {
        config = {};
        overlays = [];
      };
    in import ../external/home-manager.nix {
      inherit (pkgs) fetchpatch runCommand;
    };

in {
  buildHomeManagerConfiguration = conf: (import "${homeManager}/home-manager/home-manager.nix" {
    confPath = conf;
    confAttr = "";
  }).activationPackage;
}
