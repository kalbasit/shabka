{ config, lib, ... }:

with lib;

let cfg = config.mine.virtualisation.docker;
in {
  options.mine.virtualisation.docker.enable = mkEnableOption "Enable Docker";

  config = mkIf cfg.enable {
    # Enable docker support
    virtualisation.docker.enable = true;
  };
}
