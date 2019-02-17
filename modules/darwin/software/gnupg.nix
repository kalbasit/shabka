{ config, lib, ... }:

with lib;

{
  options.mine.gnupg.enable = mkEnableOption "Enable GnuPG";

  config = mkIf config.mine.gnupg.enable {
    programs.gnupg = {
      enable = true;
      enableSSHSupport = true;
    };
  };
}
