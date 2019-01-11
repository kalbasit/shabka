{ config, pkgs, lib, ... }:

with lib;

{
  options.mine.keybase.enable = mkEnableOption "Enable keybase service";

  config = mkIf config.mine.keybase.enable {
    services.kbfs.enable = true;
    services.keybase.enable = true;
  };
}
