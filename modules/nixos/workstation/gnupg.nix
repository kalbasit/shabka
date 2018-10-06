{ config, lib, ... }:

with lib;

{
  options.mine.workstation.gnupg.enable = mkEnableOption "Enable GnuPG";

  config = mkIf config.mine.workstation.gnupg.enable {
    programs.gnupg.agent.enable = true;
    programs.gnupg.agent.enableBrowserSocket = true;
  };
}

