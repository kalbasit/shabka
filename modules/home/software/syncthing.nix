{ config, pkgs, lib, ... }:

with lib;

let
  cfg = config.shabka.syncthing;
in {
  options.shabka.syncthing.enable = mkEnableOption "Enable syncthing service";

  config = mkIf cfg.enable {
    services.syncthing.enable = true;
  };
}
