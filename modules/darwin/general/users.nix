# TODO(low): follow the same structure as NixOS, maybe with a common location?

{ config, pkgs, lib, ... }:

with lib;

let
  makeUser = userName: { home ? "/Users/${userName}" }: nameValuePair
    userName
    { inherit home; };

  makeHM = userName: { }: nameValuePair
    userName
    (config.shabka.home-manager.config {
      darwinConfig = config;
    });

  defaultUsers = {
    yl = {};
  };

in {
  users.users = (mapAttrs' makeUser defaultUsers);
  home-manager.users = mapAttrs' makeHM defaultUsers;
  system.activationScripts.postActivation.text = ''
    sudo chsh -s ${getBin pkgs.zsh}/bin/zsh yl
  '';
}
