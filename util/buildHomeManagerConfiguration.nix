{ ... }:

{
  buildHomeManagerConfiguration = conf: (import <home-manager/home-manager/home-manager.nix> {
    confPath = conf;
    confAttr = "";
  }).activationPackage;
}
