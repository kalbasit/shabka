{ config, lib, ... }:

with lib;

{
  config = mkIf (config.shabka.darwinConfig != {}) {
    home.file.".config/karabiner/karabiner.json".source = ./karabiner.json;
  };
}
