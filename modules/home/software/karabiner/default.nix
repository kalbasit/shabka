{ config, lib, ... }:

with lib;

let
  cfg = config.shabka.karabiner;
in {
  options.shabka.karabiner.enable = mkEnableOption "Enable karabiner (only relevant for Darwin hosts).";

  config = mkIf cfg.enable {
    assertions = [
      {
        assertion = config.shabka.darwinConfig != {};
        message = "Karabiner isn't used on Linux hosts. It should thus be disabled on them.";
      }
    ];
    home.file.".config/karabiner/karabiner.json".source = ./karabiner.json;
  };
}
