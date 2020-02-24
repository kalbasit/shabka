{ pkgs, lib }:

let
  shabka = import <shabka> {};
  release = builtins.getEnv "RELEASE";
in {
  buildHomeManagerConfiguration = conf: (import "${shabka.external.home-manager."${release}".path}/home-manager/home-manager.nix" {
    confPath = conf;
    confAttr = "";
  }).activationPackage;
}
