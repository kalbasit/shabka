{ lib, ... }:

with lib;

let
  pinnedHM = import ../../../external/home-manager.nix;
in {
  imports = [
    (import pinnedHM {}).nixos

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
