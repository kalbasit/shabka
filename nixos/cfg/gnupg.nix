{ config, lib, ... }:

with lib;

let
  cfg = config.mine.gnupg;
in {
  options.mine.gnupg.enable = mkEnableOption "Enable GnuPG";
  config = mkIf cfg.enable {
    programs.gnupg.agent.enable = true;
    programs.gnupg.agent.enableBrowserSocket = true;
  };
}
