# fetchpatch and runCommand are causing an infinite loop here
{ fetchpatch, runCommand, lib, ... }:

with lib;

let
  homeManager = import ../../../external/home-manager.nix {
    inherit fetchpatch runCommand;
  };
in {
  imports = [
    (import "${homeManager}/nixos")
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
