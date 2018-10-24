{ lib, ... }:

with lib;

{
  imports = [
    (import <home-manager> {}).nixos

    # load the following when running a VM
    # ("${builtins.fetchTarball https://github.com/rycee/home-manager/archive/nixos-module-user-pkgs.tar.gz}/nixos")
  ];

  options.mine.home-manager.config = mkOption {
    default = { name, uid, isAdmin, nixosConfig }: {...}: {};
    description = ''
      Function that returns the Home Manager configuration.
    '';
  };
}
