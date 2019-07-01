{ config, pkgs, lib, ... }:

with lib;
with import ../../../util;

let
  cfg = config.mine.workstation.keeptruckin;
in {
  options.mine.workstation.keeptruckin = {
    enable = mkEnableOption "Enable KeepTruckin";
  };

  config = mkIf cfg.enable {
    # Add the extra hosts
    networking.extraHosts = ''
      127.0.0.1 docker.keeptruckin.dev docker.keeptruckin.work
    '';
  };
}
