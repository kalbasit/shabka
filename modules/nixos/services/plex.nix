{ config, pkgs, lib, ... }:

with lib;

let
  cfg = config.shabka.plex;
in {
  options.shabka.plex = {
    enable = mkEnableOption "Enable Plex service";
    dataDir = mkOption {
      type = types.str;
      description = "The path to Plex's data directory";
    };
  };

  config = mkIf cfg.enable {
    # Plex service
    services.plex = {
      inherit (cfg) dataDir;

      enable = true;
      openFirewall = true;
    };
  };
}
