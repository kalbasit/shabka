{ config, pkgs, lib, ... }:

with lib;

{
  options.shabka.keybase.enable = mkEnableOption "Enable keybase service";

  config = mkIf config.shabka.keybase.enable {
    services.kbfs.enable = true;
    services.keybase.enable = true;
  };
}
