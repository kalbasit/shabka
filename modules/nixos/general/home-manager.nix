{ lib, ... }:

with lib;

{
  imports = [
    (import <home-manager> {}).nixos
  ];

  options.mine.home-manager.config = mkOption {
    default = { name, uid, isAdmin, nixosConfig }: {...}: {};
    description = ''
      Function that returns the Home Manager configuration.
    '';
  };
}
