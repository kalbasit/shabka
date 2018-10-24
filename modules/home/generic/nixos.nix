{ lib, ... }:

with lib;

{
  options.mine.nixosConfig = mkOption {
    default = {};
    defaultText = ''
      NixOS configuration. On NixOS machines, it should be the config itself.
      On non-NixOS machines, all the required keys must be set manually.
    '';
  };
}
