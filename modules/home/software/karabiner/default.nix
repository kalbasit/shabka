{ config, lib, ... }:

with lib;

{
  config = mkIf (config.mine.darwinConfig != {}) {
    home.file.".config/karabiner/karabiner.json".source = ./karabiner.json;
  };
}
