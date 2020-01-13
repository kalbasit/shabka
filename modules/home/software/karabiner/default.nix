{ config, lib, ... }:

with lib;

let
  cfg = config.shabka.karabiner;
in {
  options.shabka.karabiner.enable = mkEnableOption "Enable karabiner (only relevant for Darwin hosts).";

  config = mkIf (cfg.enable && (config.shabka.darwinConfig != {})) {
    home.file.".config/karabiner/karabiner.json".source = ./karabiner.json;
  };
}
