{ lib, ... }:

with lib;

{
  options.shabka.home-manager.config = mkOption {
    default = { name, uid, isAdmin, nixosConfig }: {...}: {};
    description = ''
      Function that returns the Home Manager configuration.
    '';
  };

  config = {
    home-manager.useUserPackages = true;
  };
}
