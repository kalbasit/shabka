{ lib, ... }:

with lib;

{
  imports = [
    (import <home-manager> {}).nixos
  ];

  options.mine.home-manager.config = mkOption {
    default = { ... }: {};
    description = ''
        Per-user Home Manager configuration.
    '';
  };
}
