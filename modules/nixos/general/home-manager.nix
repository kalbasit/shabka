{ lib, ... }:

with lib;

{
  imports = [
    (import <home-manager> {}).nixos
  ];

  options.mine.home-manager.users = mkOption mkOption {
    type = types.attrsOf hmModule;
    default = {};
    description = ''
        Per-user Home Manager configuration.
    '';
  };
}
