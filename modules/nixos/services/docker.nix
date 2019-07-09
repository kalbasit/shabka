{ config, lib, ... }:

with lib;

let cfg = config.shabka.virtualisation.docker;
in {
  options.shabka.virtualisation.docker.enable = mkEnableOption "Enable Docker";

  config = mkIf cfg.enable {
    virtualisation.docker.enable = true;
    shabka.users.groups = ["docker"];
  };
}
