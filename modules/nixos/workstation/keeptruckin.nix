{ config, pkgs, lib, ... }:

with lib;

let
  cfg = config.shabka.workstation.keeptruckin;
in {
  options.shabka.workstation.keeptruckin = {
    enable = mkEnableOption "Enable KeepTruckin";
  };

  config = mkIf cfg.enable {
    # Add the extra hosts
    networking.extraHosts = ''
      127.0.0.1 docker.keeptruckin.dev docker.keeptruckin.work
    '';
  };
}
