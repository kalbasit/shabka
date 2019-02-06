{ pkgs, lib }:

let
  homeManager = import ../external/home-manager.nix {
    inherit pkgs;
    inherit (import ./assertMsg.nix { inherit lib; }) assertMsg;
  };

in {
  buildHomeManagerConfiguration = conf: (import "${homeManager}/home-manager/home-manager.nix" {
    confPath = conf;
    confAttr = "";
  });
}
