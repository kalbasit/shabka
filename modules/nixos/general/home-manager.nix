{ lib, shabka ? import <shabka> { }, ... }:

with lib;

{
  imports = [
    (import "${shabka.external.home-manager.path}/nixos")
  ];

  options.mine.home-manager.config = mkOption {
    default = { name, uid, isAdmin, nixosConfig }: {...}: {};
    description = ''
      Function that returns the Home Manager configuration.
    '';
  };

  config = {
    home-manager.useUserPackages = true;
  };
}
