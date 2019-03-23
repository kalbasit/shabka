{ pkgs, lib }:

let
  external = import ../../external {};

in {
  buildHomeManagerConfiguration = conf: (import "${external.home-manager.path}/home-manager/home-manager.nix" {
    confPath = conf;
    confAttr = "";
  }).activationPackage;
}
