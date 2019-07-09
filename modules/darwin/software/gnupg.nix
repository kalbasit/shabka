{ config, lib, ... }:

with lib;

{
  options.shabka.gnupg.enable = mkEnableOption "Enable GnuPG";

  config = mkIf config.shabka.gnupg.enable {
    programs.gnupg = {
      agent.enable = true;
      agent.enableSSHSupport = true;
    };
  };
}
