{ lib, ... }:

with lib;

{
  options.mine.nixosConfig = mkOption {
    default = {};
    defaultText = ''
      NixOS configuration, set only on NixOS machines
    '';
  };
}
